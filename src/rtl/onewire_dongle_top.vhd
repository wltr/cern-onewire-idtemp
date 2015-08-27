-------------------------------------------------------------------------------
--! @file      onewire_dongle_top.vhd
--! @author    Johannes Walter <johannes@greenshire.io>
--! @copyright LGPL v2.1
--! @brief     1-wire ID and temperature sensor USB dongle.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library proasic3;
use proasic3.all;

--! @brief   Entity declaration of onewire_dongle_top
--! @details
--! When push-button is pressed, read 1-wire bus and transmit all values over
--! UART at 115200 baud.
entity onewire_dongle_top is
  port (
    --! @name Clock and resets
    --! @{

    --! System clock
    clk_pad_i   : in std_ulogic;
    --! Asynchronous active-low reset
    rst_asy_n_i : in std_ulogic;

    --! @}
    --! @name Push-button and LEDs
    --! @{

    --! Push-button (active-low)
    pb_n_i : in  std_ulogic;
    --! LEDs
    leds_o : out std_ulogic_vector(2 downto 0);

    --! @}
    --! @name UART
    --! @{

    --! Receive signal
    uart_rx_i : in  std_ulogic;
    --! Transmit signal
    uart_tx_o : out std_ulogic;

    --! @}
    --! @name 1-wire bus
    --! @{

    --! Receive signal
    ow_rx_i   : in  std_ulogic;
    --! Transmit signal
    ow_tx_o   : out std_ulogic;
    --! Strong pull-up
    ow_spup_o : out std_ulogic);

    --! @}
end entity onewire_dongle_top;

--! RTL implementation of onewire_dongle_top
architecture rtl of onewire_dongle_top is

  -----------------------------------------------------------------------------
  --! @name Components
  -----------------------------------------------------------------------------
  --! @{

  -- Input buffer to force INBUF on clock (can't use pin 10 otherwise)
  component INBUF_LVCMOS33
    port (
      PAD : in  std_logic;
      Y   : out std_logic);
  end component;

  --! @}
  -----------------------------------------------------------------------------
  --! @name Types and Constants
  -----------------------------------------------------------------------------
  --! @{

  type state_t is (IDLE, FULL_RUN, GET_DATA, DATA);

  type reg_t is record
    state : state_t;
    rd    : std_ulogic;
    addr  : unsigned(4 downto 0);
  end record;

  constant init_c : reg_t := (
    state => IDLE,
    rd    => '0',
    addr  => "00000");

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

  signal clk_i : std_ulogic;
  signal rst_n : std_ulogic;

  signal pb_n    : std_ulogic;
  signal ow_rx   : std_ulogic;
  signal uart_rx : std_ulogic;

  signal ow_discover       : std_ulogic;
  signal ow_get_temp       : std_ulogic;
  signal ow_busy           : std_ulogic;
  signal ow_done           : std_ulogic;
  signal ow_device_count   : std_ulogic_vector(4 downto 0);
  signal ow_error_too_many : std_ulogic;

  signal ow_rd_addr    : std_ulogic_vector(4 downto 0);
  signal ow_rd_en      : std_ulogic;
  signal ow_rd_data    : std_ulogic_vector(63 downto 0);
  signal ow_rd_data_en : std_ulogic;

  signal tx_done : std_ulogic;

  signal uart_data    : std_ulogic_vector(7 downto 0);
  signal uart_data_en : std_ulogic;
  signal uart_done    : std_ulogic;

  signal uart_cmd    : std_ulogic_vector(7 downto 0);
  signal uart_cmd_en : std_ulogic;

  signal trigger_discover : std_ulogic;
  signal trigger_get_temp : std_ulogic;

  signal nxt_reg : reg_t;

  --! @}

begin -- architecture rtl

  -----------------------------------------------------------------------------
  -- Outputs
  -----------------------------------------------------------------------------

  leds_o(0) <= ow_busy;
  leds_o(1) <= '0' when ow_device_count = "00000" else '1';
  leds_o(2) <= ow_error_too_many;

  -----------------------------------------------------------------------------
  -- Signal Assignments
  -----------------------------------------------------------------------------

  trigger_discover <= '1' when uart_cmd = x"01" and uart_cmd_en = '1' else '0';
  trigger_get_temp <= '1' when uart_cmd = x"10" and uart_cmd_en = '1' else '0';

  -----------------------------------------------------------------------------
  -- Instantiations
  -----------------------------------------------------------------------------

  -- Input buffer to force INBUF on clock (can't use pin 10 otherwise)
  INBUF_inst : INBUF_LVCMOS33
    port map (
      PAD => clk_pad_i,
      Y   => clk_i);

  reset_gen_inst : entity work.reset_generator
    port map (
      clk_i     => clk_i,
      rst_asy_i => rst_asy_n_i,
      rst_o     => rst_n);

  ext_inputs_inst : entity work.external_inputs
    generic map (
      init_value_g => '1',
      num_inputs_g => 3,
      filter_g     => false)
    port map (
      clk_i       => clk_i,
      rst_asy_n_i => rst_n,
      rst_syn_i   => '0',

      sig_i(0) => pb_n_i,
      sig_i(1) => ow_rx_i,
      sig_i(2) => uart_rx_i,

      sig_o(0) => pb_n,
      sig_o(1) => ow_rx,
      sig_o(2) => uart_rx);

  onewire_idtemp_inst : entity work.onewire_idtemp
    generic map (
      clk_frequency_g => 40e6,
      max_devices_g   => 16,
      invert_bus_g    => true,
      invert_pullup_g => true)
    port map (
      clk_i       => clk_i,
      rst_asy_n_i => rst_n,
      rst_syn_i   => '0',

      discover_i       => ow_discover,
      get_temp_i       => ow_get_temp,
      busy_o           => ow_busy,
      done_o           => ow_done,
      device_count_o   => ow_device_count,
      error_too_many_o => ow_error_too_many,

      rd_addr_i    => ow_rd_addr,
      rd_en_i      => ow_rd_en,
      rd_data_o    => ow_rd_data,
      rd_data_en_o => ow_rd_data_en,
      rd_busy_o    => open,

      strong_pullup_o => ow_spup_o,
      rx_i            => ow_rx,
      tx_o            => ow_tx_o);

  array_tx_inst : entity work.array_tx
    generic map (
      data_count_g => 8,
      data_width_g => 8)
    port map (
      clk_i       => clk_i,
      rst_asy_n_i => rst_n,
      rst_syn_i   => '0',

      data_i    => ow_rd_data,
      data_en_i => ow_rd_data_en,
      busy_o    => open,
      done_o    => tx_done,

      tx_data_o    => uart_data,
      tx_data_en_o => uart_data_en,
      tx_done_i    => uart_done);

  uart_tx_inst : entity work.uart_tx
    generic map (
      data_width_g => 8,
      parity_g     => 0,
      stop_bits_g  => 1,
      num_ticks_g  => 347)
    port map (
      clk_i       => clk_i,
      rst_asy_n_i => rst_n,
      rst_syn_i   => '0',

      data_i    => uart_data,
      data_en_i => uart_data_en,
      busy_o    => open,
      done_o    => uart_done,

      tx_o => uart_tx_o);

  uart_rx_inst : entity work.uart_rx
    generic map (
      data_width_g => 8,
      parity_g     => 0,
      stop_bits_g  => 1,
      num_ticks_g  => 347)
    port map (
      clk_i       => clk_i,
      rst_asy_n_i => rst_n,
      rst_syn_i   => '0',

      rx_i => uart_rx,

      data_o    => uart_cmd,
      data_en_o => uart_cmd_en,
      error_o   => open);

  -----------------------------------------------------------------------------
  -- Registers
  -----------------------------------------------------------------------------

  regs : process (clk_i, rst_n) is
    procedure reset is
    begin
      reg <= init_c;
    end procedure reset;
  begin -- process regs
    if rst_n = '0' then
      reset;
    elsif rising_edge(clk_i) then
      reg <= nxt_reg;
    end if;
  end process regs;

  -----------------------------------------------------------------------------
  -- Combinatorics
  -----------------------------------------------------------------------------

  comb : process (reg, pb_n, ow_busy, ow_done, tx_done, trigger_discover,
    trigger_get_temp) is
  begin -- process comb
    -- Defaults
    nxt_reg <= reg;

    nxt_reg.rd <= '0';

    ow_discover <= '0';
    ow_get_temp <= '0';
    ow_rd_addr <= std_ulogic_vector(reg.addr);
    ow_rd_en   <= reg.rd;

    case reg.state is
      when IDLE =>
        if pb_n = '0' and ow_busy = '0' then
          ow_discover <= '1';
          nxt_reg.state <= FULL_RUN;
        elsif trigger_discover = '1' and ow_busy = '0' then
          ow_discover <= '1';
          nxt_reg.state <= GET_DATA;
        elsif trigger_get_temp = '1' and ow_busy = '0' then
          ow_get_temp <= '1';
          nxt_reg.state <= GET_DATA;
        end if;

      when FULL_RUN =>
        if ow_done = '1' then
          ow_get_temp <= '1';
          nxt_reg.state <= GET_DATA;
        end if;

      when GET_DATA =>
        if ow_done = '1' then
          nxt_reg.rd <= '1';
          nxt_reg.state <= DATA;
        end if;

      when DATA =>
        if tx_done = '1' then
          if reg.addr = "11111" then
            nxt_reg <= init_c;
          else
            nxt_reg.addr <= reg.addr + 1;
            nxt_reg.rd <= '1';
          end if;
        end if;

    end case;
  end process comb;

end architecture rtl;
