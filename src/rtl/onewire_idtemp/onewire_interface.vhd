-------------------------------------------------------------------------------
--! @file      onewire_interface.vhd
--! @author    Johannes Walter <johannes@greenshire.io>
--! @copyright LGPL v2.1
--! @brief     1-wire bus interface.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

library work;
use work.lfsr_pkg.all;

--! @brief Entity declaration of onewire_interface
entity onewire_interface is
  generic (
    --! System clock frequency in Hz
    clk_frequency_g : natural := 40e6);
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

    --! Send a bus reset command
    bus_rst_i : in  std_ulogic;
    --! Send data bit
    send_i    : in  std_ulogic;
    --! The data bit to be sent
    data_i    : in  std_ulogic;
    --! Receive data bit
    recv_i    : in  std_ulogic;
    --! The received data bit
    data_o    : out std_ulogic;
    --! The received data bit enable
    data_en_o : out std_ulogic;
    --! Done flag
    done_o    : out std_ulogic;

    --! @}
    --! @name External signals
    --! @{

    --! Receiving bus input
    rx_i : in  std_ulogic;
    --! Transmitting bus output
    tx_o : out std_ulogic);

    --! @}
end entity onewire_interface;

--! RTL implementation of onewire_interface
architecture rtl of onewire_interface is

  -----------------------------------------------------------------------------
  --! @name Types and Constants
  -----------------------------------------------------------------------------
  --! @{

  --! Time to start a read or write operation in seconds
  constant t_rw_start_c : real := 0.000005;
  --! Time to wait until input is sampled during a read operation in seconds
  constant t_rw_smpl_c  : real := 0.000010;
  --! Time to hold the state during a write operation or wait during a read operation in seconds
  constant t_rw_hold_c  : real := 0.00006;
  --! Time to recover from a read or write operation in seconds
  constant t_rw_recvr_c : real := t_rw_hold_c + 0.00001;

  --! Time to start a reset command in seconds
  constant t_rst_start_c : real := 0.0005;
  --! Time to wait until presence pulse is sampled in seconds
  constant t_rst_smpl_c  : real := 0.00057;
  --! Total length of reset command in seconds
  constant t_rst_end_c   : real := 0.001;

  constant clk_period_c : real := 1.0 / real(clk_frequency_g);

  constant cnt_rw_start_c : natural := natural(ceil(t_rw_start_c / clk_period_c));
  constant cnt_rw_recvr_c : natural := natural(ceil(t_rw_recvr_c / clk_period_c));
  constant cnt_rw_hold_c  : natural := natural(ceil(t_rw_hold_c / clk_period_c));
  constant cnt_rw_smpl_c  : natural := natural(ceil(t_rw_smpl_c / clk_period_c));

  constant cnt_rst_start_c : natural := natural(ceil(t_rst_start_c / clk_period_c));
  constant cnt_rst_smpl_c  : natural := natural(ceil(t_rst_smpl_c / clk_period_c));
  constant cnt_rst_end_c   : natural := natural(ceil(t_rst_end_c / clk_period_c));

  constant lfsr_len_c : natural := lfsr_length(cnt_rst_end_c);
  subtype lfsr_t is std_ulogic_vector(lfsr_len_c - 1 downto 0);

  constant lfsr_seed_c : lfsr_t := lfsr_seed(lfsr_len_c);

  constant max_rw_start_c : lfsr_t := x"CC73"; --lfsr_shift(lfsr_seed_c, cnt_rw_start_c - 1);
  constant max_rw_recvr_c : lfsr_t := x"6EA4"; --lfsr_shift(lfsr_seed_c, cnt_rw_recvr_c - 1);
  constant max_rw_hold_c  : lfsr_t := x"EE75"; --lfsr_shift(lfsr_seed_c, cnt_rw_hold_c - 1);
  constant max_rw_smpl_c  : lfsr_t := x"8C97"; --lfsr_shift(lfsr_seed_c, cnt_rw_smpl_c - 1);

  constant max_rst_start_c : lfsr_t := x"FD03"; --lfsr_shift(lfsr_seed_c, cnt_rst_start_c - 1);
  constant max_rst_smpl_c  : lfsr_t := x"672B"; --lfsr_shift(lfsr_seed_c, cnt_rst_smpl_c - 1);
  constant max_rst_end_c   : lfsr_t := x"0170"; --lfsr_shift(lfsr_seed_c, cnt_rst_end_c - 1);

  type state_t is (IDLE, RESET, SEND, RECEIVE);

  type reg_t is record
    state   : state_t;
    lfsr    : lfsr_t;
    tx      : std_ulogic;
    done    : std_ulogic;
    data    : std_ulogic;
    data_en : std_ulogic;
  end record;

  constant init_c : reg_t := (
    state   => IDLE,
    lfsr    => lfsr_seed_c,
    tx      => '1',
    done    => '0',
    data    => '0',
    data_en => '0');

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

  signal nxt_reg : reg_t;

  --! @}

begin -- architecture rtl

  -----------------------------------------------------------------------------
  -- Outputs
  -----------------------------------------------------------------------------

  data_o    <= reg.data;
  data_en_o <= reg.data_en;
  done_o    <= reg.done;

  tx_o <= reg.tx;

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

  comb : process (reg, bus_rst_i, send_i, recv_i, data_i, rx_i) is
  begin -- process comb
    -- Defaults
    nxt_reg <= reg;

    nxt_reg.done    <= init_c.done;
    nxt_reg.data_en <= init_c.data_en;

    case reg.state is
      when IDLE =>
        if bus_rst_i = '1' then
          nxt_reg.state <= RESET;
          nxt_reg.tx    <= '0';
        elsif send_i = '1' then
          nxt_reg.state <= SEND;
          nxt_reg.tx   <= '0';
          nxt_reg.data <= data_i;
        elsif recv_i = '1' then
          nxt_reg.state <= RECEIVE;
          nxt_reg.tx    <= '0';
        end if;

      when RESET =>
        nxt_reg.lfsr <= lfsr_shift(reg.lfsr);
        if reg.lfsr = max_rst_start_c then
          nxt_reg.tx <= '1';
        end if;
        if reg.lfsr = max_rst_smpl_c then
          nxt_reg.data <= rx_i;
        end if;
        if reg.lfsr = max_rst_end_c then
          nxt_reg      <= init_c;
          nxt_reg.done <= '1';
          nxt_reg.data <= reg.data;
        end if;

      when SEND =>
        nxt_reg.lfsr <= lfsr_shift(reg.lfsr);
        if reg.lfsr = max_rw_start_c then
          nxt_reg.tx <= reg.data;
        end if;
        if reg.lfsr = max_rw_hold_c then
          nxt_reg.tx <= '1';
        end if;
        if reg.lfsr = max_rw_recvr_c then
          nxt_reg      <= init_c;
          nxt_reg.done <= '1';
        end if;

      when RECEIVE =>
        nxt_reg.lfsr <= lfsr_shift(reg.lfsr);
        if reg.lfsr = max_rw_start_c then
          nxt_reg.tx <= '1';
        end if;
        if reg.lfsr = max_rw_smpl_c then
          nxt_reg.data <= rx_i;
        end if;
        if reg.lfsr = max_rw_recvr_c then
          nxt_reg         <= init_c;
          nxt_reg.done    <= '1';
          nxt_reg.data    <= reg.data;
          nxt_reg.data_en <= '1';
        end if;

    end case;
  end process comb;

end architecture rtl;
