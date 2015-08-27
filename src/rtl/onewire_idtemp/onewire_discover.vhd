-------------------------------------------------------------------------------
--! @file      onewire_discover.vhd
--! @author    Johannes Walter <johannes@greenshire.io>
--! @copyright LGPL v2.1
--! @brief     Perform a search algorithm to discover devices on the bus.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.lfsr_pkg.all;
use work.onewire_idtemp_pkg.all;

--! @brief Entity declaration of onewire_discover
entity onewire_discover is
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
    --! @name Internal signals
    --! @{

    --! Start search algorithm
    discover_i  : in  std_ulogic;
    --! Discovered device ID
    id_o        : out std_ulogic_vector(63 downto 0);
    --! Discovered device ID enable
    id_en_o     : out std_ulogic;
    --! Done flag
    done_o      : out std_ulogic;

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
end entity onewire_discover;

--! RTL implementation of onewire_discover
architecture rtl of onewire_discover is

  -----------------------------------------------------------------------------
  --! @name Types and Constants
  -----------------------------------------------------------------------------
  --! @{

  constant lfsr_len_c : natural := lfsr_length(cmd_search_c'length);
  subtype lfsr_t is std_ulogic_vector(lfsr_len_c - 1 downto 0);

  constant lfsr_seed_c : lfsr_t := lfsr_seed(lfsr_len_c);
  constant lfsr_max_c  : lfsr_t := lfsr_shift(lfsr_seed_c, cmd_search_c'length - 1);

  type state_t is (IDLE, RESET_DONE, SEARCH_COMMAND, READ_ID_BIT,
    READ_CMP_ID_BIT, COMPARE, CHECK);

  type reg_t is record
    state            : state_t;
    lfsr             : lfsr_t;
    done             : std_ulogic;
    id               : std_ulogic_vector(id_o'high + 1 downto id_o'low + 1);
    id_en            : std_ulogic;
    cmd              : std_ulogic_vector(7 downto 0);
    bus_rst          : std_ulogic;
    bit_send         : std_ulogic;
    bit_recv         : std_ulogic;
    crc_reset        : std_ulogic;
    id_bit           : std_ulogic;
    cmp_id_bit       : std_ulogic;
    search           : std_ulogic;
    id_bit_number    : unsigned(6 downto 0);
    marker           : unsigned(6 downto 0);
    last_discrepancy : unsigned(6 downto 0);
  end record;

  constant init_c : reg_t := (
    state            => IDLE,
    lfsr             => lfsr_seed_c,
    done             => '0',
    id               => (others => '0'),
    id_en            => '0',
    cmd              => cmd_search_c,
    bus_rst          => '0',
    bit_send         => '0',
    bit_recv         => '0',
    crc_reset        => '0',
    id_bit           => '0',
    cmp_id_bit       => '0',
    search           => '0',
    id_bit_number    => to_unsigned(1, 7),
    marker           => to_unsigned(0, 7),
    last_discrepancy => to_unsigned(0, 7));

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

  id_o       <= reg.id;
  id_en_o    <= reg.id_en;
  done_o     <= reg.done;
  bus_rst_o  <= reg.bus_rst;
  bit_send_o <= reg.bit_send;
  bit_recv_o <= reg.bit_recv;
  bit_o      <= reg.search;

  -----------------------------------------------------------------------------
  -- Instantiations
  -----------------------------------------------------------------------------

  crc_inst : entity work.onewire_crc
    port map (
      clk_i       => clk_i,
      rst_asy_n_i => rst_asy_n_i,
      rst_syn_i   => rst_syn_i,

      reset_i   => reg.crc_reset,
      data_i    => reg.search,
      data_en_i => reg.bit_send,
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

  comb : process (reg, discover_i, bit_i, bit_en_i, bit_done_i, crc_valid) is
  begin -- process comb
    -- Defaults
    nxt_reg <= reg;

    nxt_reg.done      <= init_c.done;
    nxt_reg.id_en     <= init_c.id_en;
    nxt_reg.bus_rst   <= init_c.bus_rst;
    nxt_reg.bit_send  <= init_c.bit_send;
    nxt_reg.bit_recv  <= init_c.bit_recv;
    nxt_reg.crc_reset <= init_c.crc_reset;

    case reg.state is
      when IDLE =>
        if discover_i = '1' then
          nxt_reg.bus_rst <= '1';
          nxt_reg.state   <= RESET_DONE;
        end if;

      when RESET_DONE =>
        if bit_done_i = '1' then
          if bit_i = '1' then
            nxt_reg      <= init_c;
            nxt_reg.done <= '1';
          else
            nxt_reg.bit_send <= '1';
            nxt_reg.search   <= reg.cmd(reg.cmd'low);
            nxt_reg.cmd      <= '0' & reg.cmd(reg.cmd'high downto reg.cmd'low + 1);
            nxt_reg.state    <= SEARCH_COMMAND;
          end if;
        end if;

      when SEARCH_COMMAND =>
        if bit_done_i = '1' then
          if reg.lfsr = lfsr_max_c then
            nxt_reg.bit_recv  <= '1';
            nxt_reg.crc_reset <= '1';
            nxt_reg.state     <= READ_ID_BIT;
          else
            nxt_reg.bit_send <= '1';
            nxt_reg.search   <= reg.cmd(reg.cmd'low);
            nxt_reg.cmd      <= '0' & reg.cmd(reg.cmd'high downto reg.cmd'low + 1);
            nxt_reg.lfsr     <= lfsr_shift(reg.lfsr);
          end if;
        end if;

      when READ_ID_BIT =>
        if bit_en_i = '1' then
          nxt_reg.id_bit   <= bit_i;
          nxt_reg.bit_recv <= '1';
          nxt_reg.state    <= READ_CMP_ID_BIT;
        end if;

      when READ_CMP_ID_BIT =>
        if bit_en_i = '1' then
          nxt_reg.cmp_id_bit <= bit_i;
          nxt_reg.state      <= COMPARE;
        end if;

      when COMPARE =>
        nxt_reg.state    <= CHECK;
        nxt_reg.bit_send <= '1';
        if reg.id_bit = '1' and reg.cmp_id_bit = '1' then
          nxt_reg      <= init_c;
          nxt_reg.done <= '1';
        elsif reg.id_bit = '0' and reg.cmp_id_bit = '0' then
          if reg.id_bit_number = reg.last_discrepancy then
            nxt_reg.id(to_integer(reg.id_bit_number)) <= '1';
            nxt_reg.search <= '1';
          elsif reg.id_bit_number > reg.last_discrepancy then
            nxt_reg.id(to_integer(reg.id_bit_number)) <= '0';
            nxt_reg.search <= '0';
            nxt_reg.marker <= reg.id_bit_number;
          else
            nxt_reg.search <= reg.id(to_integer(reg.id_bit_number));
            if reg.id(to_integer(reg.id_bit_number)) = '0' then
              nxt_reg.marker <= reg.id_bit_number;
            end if;
          end if;
        else
          nxt_reg.id(to_integer(reg.id_bit_number)) <= reg.id_bit;
          nxt_reg.search <= reg.id_bit;
        end if;

      when CHECK =>
        if bit_done_i = '1' then
          if to_integer(reg.id_bit_number) < reg.id'length then
            nxt_reg.id_bit_number <= reg.id_bit_number + 1;
            nxt_reg.bit_recv      <= '1';
            nxt_reg.state         <= READ_ID_BIT;
          else
            if to_integer(reg.marker) = 0 then
              nxt_reg      <= init_c;
              nxt_reg.done <= '1';
            else
              nxt_reg                  <= init_c;
              nxt_reg.last_discrepancy <= reg.marker;
              nxt_reg.bus_rst          <= '1';
              nxt_reg.state            <= RESET_DONE;
            end if;
            if crc_valid = '0' then
              nxt_reg.id <= (others => '1');
            else
              nxt_reg.id    <= reg.id;
            end if;
            nxt_reg.id_en <= '1';
          end if;
        end if;

    end case;
  end process comb;

end architecture rtl;
