-------------------------------------------------------------------------------
--! @file      onewire_control.vhd
--! @author    Johannes Walter <johannes@greenshire.io>
--! @copyright LGPL v2.1
--! @brief     Control all 1-wire interfaces.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.lfsr_pkg.all;
use work.onewire_idtemp_pkg.all;

--! @brief Entity declaration of onewire_control
entity onewire_control is
  generic (
    --! System clock frequency in Hz
    clk_frequency_g : natural := 40e6;
    --! Maximum number of devices on a bus
    max_devices_g : positive := 16);
  port (
    --! @name Clock and resets
    --! @{

    --! System clock
    clk_i       : in std_ulogic;
    --! Asynchronous active-low reset
    rst_asy_n_i : in std_ulogic;
    --! Synchronous active-high reset
    rst_syn_i   : in std_ulogic;

    --! @}
    --! @name Status and control signals
    --! @{

    --! Discover devices on the 1-wire bus
    discover_i       : in  std_ulogic;
    --! Convert and retrieve temperature values
    get_temp_i       : in  std_ulogic;
    --! Busy flag
    busy_o           : out std_ulogic;
    --! Done flag
    done_o           : out std_ulogic;
    --! Number of detected devices
    device_count_o   : out std_ulogic_vector(natural(ceil(log2(real(max_devices_g + 1)))) - 1 downto 0);
    --! Error flag if too many devices are detected
    error_too_many_o : out std_ulogic;
    --! Enable strong pull-up circuit to provide more current during temperature conversion
    strong_pullup_o  : out std_ulogic;

    --! @}
    --! @name Internal signals
    --! @{

    --! Start search algorithm
    discover_o  : out std_ulogic;
    --! Discovered device ID enable
    id_en_i     : in  std_ulogic;
    --! Done flag
    done_i      : in  std_ulogic;

    --! @}
    --! @name Memory interface signals
    --! @{

    --! Write address
    mem_wr_addr_o : out std_ulogic_vector(natural(ceil(log2(real(max_devices_g * 2)))) - 1 downto 0);
    --! Write enable
    mem_wr_en_o   : out std_ulogic;
    --! Data output
    mem_wr_data_o : out std_ulogic_vector(63 downto 0);
    --! Write done flag
    mem_wr_done_i : in  std_ulogic;

    --! Read address
    mem_rd_addr_o    : out std_ulogic_vector(natural(ceil(log2(real(max_devices_g * 2)))) - 1 downto 0);
    --! Read enable
    mem_rd_en_o      : out std_ulogic;
    --! Data input
    mem_rd_data_i    : in  std_ulogic_vector(63 downto 0);
    --! Data input enable
    mem_rd_data_en_i : in  std_ulogic;

    --! @}
    --! @name Bus interface signals
    --! @{

    --! Send a bus reset command
    bus_rst_o  : out std_ulogic;
    --! Send data bit
    bit_send_o : out std_ulogic;
    --! The data bit to be sent
    bit_o      : out std_ulogic;
    --! Receive data bit
    bit_recv_o : out std_ulogic;
    --! The received data bit
    bit_i      : in  std_ulogic;
    --! The received data bit enable
    bit_en_i   : in  std_ulogic;
    --! Done flag
    bit_done_i : in  std_ulogic);

    --! @}
end entity onewire_control;

--! RTL implementation of onewire_control
architecture rtl of onewire_control is

  -----------------------------------------------------------------------------
  --! @name Types and Constants
  -----------------------------------------------------------------------------
  --! @{

  --! Time to hold strong pull-up high during conversion
  constant t_pullup_c : real := 0.750;
  --! Time to recover from strong pull-up
  constant t_pullup_recvr_c : real := t_pullup_c + 0.000002;

  constant clk_period_c : real := 1.0 / real(clk_frequency_g);

  constant cnt_pullup_c       : natural := natural(ceil(t_pullup_c / clk_period_c));
  constant cnt_pullup_recvr_c : natural := natural(ceil(t_pullup_recvr_c / clk_period_c));

  constant lfsr_len_c : natural := lfsr_length(cnt_pullup_recvr_c);
  subtype lfsr_t is std_ulogic_vector(lfsr_len_c - 1 downto 0);

  constant lfsr_seed_c : lfsr_t := lfsr_seed(lfsr_len_c);

  constant max_id_c  : lfsr_t := lfsr_shift(lfsr_seed_c, mem_wr_data_o'length - 1);
  constant max_cmd_c : lfsr_t := lfsr_shift(lfsr_seed_c, cmd_match_c'length - 1);

  constant max_pullup_c       : lfsr_t := "1110100111110000010100010"; --lfsr_shift(lfsr_seed_c, cnt_pullup_c - 1);
  constant max_pullup_recvr_c : lfsr_t := "0101100010000101001011101"; --lfsr_shift(lfsr_seed_c, cnt_pullup_recvr_c - 1);

  type state_t is (IDLE, ERASE, DISCOVER, CONVERT, SKIP_COMMAND, CONVERT_COMMAND,
    WAIT_CONVERSION, WAIT_RESET, MATCH_COMMAND, GET_ID, SEND_ID, READ_COMMAND,
    SCRATCHPAD, SCRATCHPAD_CRC, SAVE_DATA, CHECK_NUM);

  type reg_t is record
    state         : state_t;
    lfsr          : lfsr_t;
    busy          : std_ulogic;
    done          : std_ulogic;
    device_count  : unsigned(device_count_o'range);
    too_many      : std_ulogic;
    strong_pullup : std_ulogic;
    discover      : std_ulogic;
    mem_addr      : unsigned(mem_wr_addr_o'range);
    mem_wr_en     : std_ulogic;
    mem_rd_en     : std_ulogic;
    crc_reset     : std_ulogic;
    bus_rst       : std_ulogic;
    bit_send      : std_ulogic;
    bit_recv      : std_ulogic;
    data          : std_ulogic_vector(mem_wr_data_o'range);
  end record;

  constant init_c : reg_t := (
    state         => IDLE,
    lfsr          => lfsr_seed_c,
    busy          => '0',
    done          => '0',
    device_count  => to_unsigned(0, device_count_o'length),
    too_many      => '0',
    strong_pullup => '0',
    discover      => '0',
    mem_addr      => to_unsigned(0, mem_wr_addr_o'length),
    mem_wr_en     => '0',
    mem_rd_en     => '0',
    crc_reset     => '0',
    bus_rst       => '0',
    bit_send      => '0',
    bit_recv      => '0',
    data          => (others => '0'));

  --! @}
  -----------------------------------------------------------------------------
  --! @name Internal Registers
  -----------------------------------------------------------------------------
  --! @{

  signal reg : reg_t;

  --! @}
  -----------------------------------------------------------------------------
  --! @name Internal Wires
  -----------------------------------------------------------------------------
  --! @{

  signal nxt_reg   : reg_t;
  signal crc_valid : std_ulogic;

  --! @}

begin -- architecture rtl

  -----------------------------------------------------------------------------
  -- Outputs
  -----------------------------------------------------------------------------

  busy_o <= reg.busy;
  done_o <= reg.done;

  device_count_o   <= std_ulogic_vector(reg.device_count);
  error_too_many_o <= reg.too_many;
  strong_pullup_o  <= reg.strong_pullup;

  discover_o  <= reg.discover;

  mem_wr_addr_o <= std_ulogic_vector(reg.mem_addr);
  mem_wr_en_o   <= reg.mem_wr_en;
  mem_wr_data_o <= reg.data;

  mem_rd_addr_o <= std_ulogic_vector(reg.mem_addr);
  mem_rd_en_o   <= reg.mem_rd_en;

  bus_rst_o  <= reg.bus_rst;
  bit_send_o <= reg.bit_send;
  bit_o      <= reg.data(reg.data'low);
  bit_recv_o <= reg.bit_recv;

  -----------------------------------------------------------------------------
  -- Instantiations
  -----------------------------------------------------------------------------

  crc_inst : entity work.onewire_crc
    port map (
      clk_i       => clk_i,
      rst_asy_n_i => rst_asy_n_i,
      rst_syn_i   => rst_syn_i,

      reset_i   => reg.crc_reset,
      data_i    => bit_i,
      data_en_i => bit_en_i,
      valid_o   => crc_valid);

  -----------------------------------------------------------------------------
  -- Registers
  -----------------------------------------------------------------------------

  regs : process (clk_i, rst_asy_n_i) is
    procedure reset is
    begin
      reg <= init_c;
    end procedure reset;
  begin -- process regs
    if rst_asy_n_i = '0' then
      reset;
    elsif rising_edge(clk_i) then
      if rst_syn_i = '1' then
        reset;
      else
        reg <= nxt_reg;
      end if;
    end if;
  end process regs;

  -----------------------------------------------------------------------------
  -- Combinatorics
  -----------------------------------------------------------------------------

  comb : process (reg, discover_i, get_temp_i, id_en_i, done_i, crc_valid,
    mem_wr_done_i, mem_rd_data_i, mem_rd_data_en_i, bit_i, bit_en_i, bit_done_i) is
  begin -- process comb
    -- Defaults
    nxt_reg <= reg;

    nxt_reg.done      <= init_c.done;
    nxt_reg.discover  <= init_c.discover;
    nxt_reg.mem_wr_en <= init_c.mem_wr_en;
    nxt_reg.mem_rd_en <= init_c.mem_rd_en;
    nxt_reg.bus_rst   <= init_c.bus_rst;
    nxt_reg.bit_send  <= init_c.bit_send;
    nxt_reg.bit_recv  <= init_c.bit_recv;
    nxt_reg.crc_reset <= init_c.crc_reset;

    case reg.state is
      when IDLE =>
        if discover_i = '1' then
          nxt_reg           <= init_c;
          nxt_reg.busy      <= '1';
          nxt_reg.mem_wr_en <= '1';
          nxt_reg.state     <= ERASE;
        elsif get_temp_i = '1' then
          nxt_reg.busy     <= '1';
          nxt_reg.bus_rst  <= '1';
          nxt_reg.mem_addr <= init_c.mem_addr;
          nxt_reg.state   <= CONVERT;
        end if;

      when ERASE =>
        if mem_wr_done_i = '1' then
          if to_integer(reg.mem_addr) < (max_devices_g * 2) - 1 then
            nxt_reg.mem_wr_en <= '1';
            nxt_reg.mem_addr  <= reg.mem_addr + 1;
          else
            nxt_reg.discover <= '1';
            nxt_reg.mem_addr <= init_c.mem_addr;
            nxt_reg.state    <= DISCOVER;
          end if;
        end if;

      when DISCOVER =>
        if id_en_i = '1' then
          nxt_reg.device_count <= reg.device_count + 1;
          nxt_reg.mem_addr     <= reg.mem_addr + 2;
          if to_integer(reg.device_count) = max_devices_g then
            nxt_reg.too_many <= '1';
          end if;
        end if;

        if done_i = '1' then
          nxt_reg.state <= IDLE;
          nxt_reg.busy  <= '0';
          nxt_reg.done  <= '1';
        end if;

      when CONVERT =>
        if bit_done_i = '1' then
          nxt_reg.bit_send         <= '1';
          nxt_reg.data(7 downto 0) <= cmd_skip_c;
          nxt_reg.state            <= SKIP_COMMAND;
        end if;

      when SKIP_COMMAND =>
        if bit_done_i = '1' then
          if reg.lfsr = max_cmd_c then
            nxt_reg.bit_send         <= '1';
            nxt_reg.data(7 downto 0) <= cmd_convert_c;
            nxt_reg.lfsr             <= init_c.lfsr;
            nxt_reg.state            <= CONVERT_COMMAND;
          else
            nxt_reg.bit_send <= '1';
            nxt_reg.data     <= '0' & reg.data(reg.data'high downto reg.data'low + 1);
            nxt_reg.lfsr     <= lfsr_shift(reg.lfsr);
          end if;
        end if;

      when CONVERT_COMMAND =>
        if bit_done_i = '1' then
          if reg.lfsr = max_cmd_c then
            nxt_reg.strong_pullup <= '1';
            nxt_reg.lfsr          <= init_c.lfsr;
            nxt_reg.state         <= WAIT_CONVERSION;
          else
            nxt_reg.bit_send <= '1';
            nxt_reg.data     <= '0' & reg.data(reg.data'high downto reg.data'low + 1);
            nxt_reg.lfsr     <= lfsr_shift(reg.lfsr);
          end if;
        end if;

      when WAIT_CONVERSION =>
        nxt_reg.lfsr <= lfsr_shift(reg.lfsr);
        if reg.lfsr = max_pullup_c then
          nxt_reg.strong_pullup <= '0';
        end if;
        if reg.lfsr = max_pullup_recvr_c then
          nxt_reg.bus_rst <= '1';
          nxt_reg.state   <= WAIT_RESET;
        end if;

      when WAIT_RESET =>
        if bit_done_i = '1' then
          nxt_reg.bit_send         <= '1';
          nxt_reg.data(7 downto 0) <= cmd_match_c;
          nxt_reg.lfsr             <= init_c.lfsr;
          nxt_reg.state            <= MATCH_COMMAND;
        end if;

      when MATCH_COMMAND =>
        if bit_done_i = '1' then
          if reg.lfsr = max_cmd_c then
            nxt_reg.mem_rd_en <= '1';
            nxt_reg.lfsr      <= init_c.lfsr;
            nxt_reg.state     <= GET_ID;
          else
            nxt_reg.bit_send <= '1';
            nxt_reg.data     <= '0' & reg.data(reg.data'high downto reg.data'low + 1);
            nxt_reg.lfsr     <= lfsr_shift(reg.lfsr);
          end if;
        end if;

      when GET_ID =>
        if mem_rd_data_en_i = '1' then
          if mem_rd_data_i(7 downto 0) = code_ds18b20_c then
            nxt_reg.data     <= mem_rd_data_i;
            nxt_reg.bit_send <= '1';
            nxt_reg.state    <= SEND_ID;
          else
            nxt_reg.mem_addr <= reg.mem_addr + 1;
            nxt_reg.state    <= CHECK_NUM;
          end if;
        end if;

      when SEND_ID =>
        if bit_done_i = '1' then
          if reg.lfsr = max_id_c then
            nxt_reg.bit_send         <= '1';
            nxt_reg.data(7 downto 0) <= cmd_read_sp_c;
            nxt_reg.lfsr             <= init_c.lfsr;
            nxt_reg.state            <= READ_COMMAND;
          else
            nxt_reg.bit_send <= '1';
            nxt_reg.data     <= '0' & reg.data(reg.data'high downto reg.data'low + 1);
            nxt_reg.lfsr     <= lfsr_shift(reg.lfsr);
          end if;
        end if;

      when READ_COMMAND =>
        if bit_done_i = '1' then
          if reg.lfsr = max_cmd_c then
            nxt_reg.bit_recv  <= '1';
            nxt_reg.crc_reset <= '1';
            nxt_reg.lfsr      <= init_c.lfsr;
            nxt_reg.state     <= SCRATCHPAD;
          else
            nxt_reg.bit_send <= '1';
            nxt_reg.data     <= '0' & reg.data(reg.data'high downto reg.data'low + 1);
            nxt_reg.lfsr     <= lfsr_shift(reg.lfsr);
          end if;
        end if;

      when SCRATCHPAD =>
        if bit_en_i = '1' then
          nxt_reg.data <= bit_i & reg.data(reg.data'high downto reg.data'low + 1);
          if reg.lfsr = max_id_c then
            nxt_reg.bit_recv  <= '1';
            nxt_reg.lfsr      <= init_c.lfsr;
            nxt_reg.state     <= SCRATCHPAD_CRC;
          else
            nxt_reg.bit_recv <= '1';
            nxt_reg.lfsr     <= lfsr_shift(reg.lfsr);
          end if;
        end if;

      when SCRATCHPAD_CRC =>
        if bit_en_i = '1' then
          if reg.lfsr = max_cmd_c then
            nxt_reg.state <= SAVE_DATA;
          else
            nxt_reg.bit_recv <= '1';
            nxt_reg.lfsr     <= lfsr_shift(reg.lfsr);
          end if;
        end if;

      when SAVE_DATA =>
        if crc_valid = '0' then
          nxt_reg.data <= (others => '1');
        end if;
        nxt_reg.mem_wr_en <= '1';
        nxt_reg.mem_addr <= reg.mem_addr + 1;
        nxt_reg.state    <= CHECK_NUM;

      when CHECK_NUM =>
        if reg.mem_addr < (max_devices_g * 2) - 1 then
          nxt_reg.mem_addr <= reg.mem_addr + 1;
          nxt_reg.bus_rst  <= '1';
          nxt_reg.state    <= WAIT_RESET;
        else
          nxt_reg.state <= IDLE;
          nxt_reg.busy  <= '0';
          nxt_reg.done  <= '1';
        end if;

    end case;
  end process comb;

end architecture rtl;
