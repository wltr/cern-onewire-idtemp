-------------------------------------------------------------------------------
--! @file      onewire_idtemp.vhd
--! @author    Johannes Walter <johannes@greenshire.io>
--! @copyright LGPL v2.1
--! @brief     1-wire ID and temperature sensor interface.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

--! @brief   Entity declaration of onewire_idtemp
--! @details Memory: 16 bytes per device = 64 bits ID + 64 bits scratch pad
entity onewire_idtemp is
  generic (
    --! System clock frequency in Hz
    clk_frequency_g : natural := 40e6;
    --! Maximum number of devices on a bus
    max_devices_g   : positive := 16;
    --! Invert 1-wire bus RX and TX signals
    invert_bus_g    : boolean := false;
    --! Invert strong pull-up signal
    invert_pullup_g : boolean := false);
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

    --! @}
    --! @name Memory interface
    --! @{

    --! Address
    rd_addr_i    : in  std_ulogic_vector(natural(ceil(log2(real(max_devices_g * 2)))) - 1 downto 0);
    --! Read enable
    rd_en_i      : in  std_ulogic;
    --! Data output
    rd_data_o    : out std_ulogic_vector(63 downto 0);
    --! Data output enable
    rd_data_en_o : out std_ulogic;
    --! Busy flag
    rd_busy_o    : out std_ulogic;

    --! @}
    --! @name Bus interface
    --! @{

    --! Enable strong pull-up circuit to provide more current during temperature conversion
    strong_pullup_o : out std_ulogic;
    --! Input
    rx_i            : in  std_ulogic;
    --! Output
    tx_o            : out std_ulogic);

    --! @}
end entity onewire_idtemp;

--! RTL implementation of onewire_idtemp
architecture rtl of onewire_idtemp is

  -----------------------------------------------------------------------------
  --! @name Internal Wires
  -----------------------------------------------------------------------------
  --! @{

  signal rx : std_ulogic;
  signal tx : std_ulogic;

  signal strong_pullup : std_ulogic;

  signal if_bus_rst    : std_ulogic;
  signal if_send       : std_ulogic;
  signal if_tx_data    : std_ulogic;
  signal if_recv       : std_ulogic;
  signal if_rx_data    : std_ulogic;
  signal if_rx_data_en : std_ulogic;
  signal if_done       : std_ulogic;

  signal mem_wr_addr : std_ulogic_vector(rd_addr_i'range);
  signal mem_wr_en   : std_ulogic;
  signal mem_wr_data : std_ulogic_vector(63 downto 0);
  signal mem_wr_done : std_ulogic;

  signal mem_rd_addr    : std_ulogic_vector(rd_addr_i'range);
  signal mem_rd_en      : std_ulogic;
  signal mem_rd_data    : std_ulogic_vector(63 downto 0);
  signal mem_rd_data_en : std_ulogic;
  signal mem_rd_busy    : std_ulogic;

  signal dcvr_start : std_ulogic;
  signal dcvr_id    : std_ulogic_vector(63 downto 0);
  signal dcvr_id_en : std_ulogic;
  signal dcvr_done  : std_ulogic;

  signal dcvr_bus_rst   : std_ulogic;
  signal dcvr_bit_send  : std_ulogic;
  signal dcvr_bit_tx    : std_ulogic;
  signal dcvr_bit_recv  : std_ulogic;
  signal dcvr_bit_rx    : std_ulogic;
  signal dcvr_bit_rx_en : std_ulogic;
  signal dcvr_bit_done  : std_ulogic;

  signal ctrl_wr_addr : std_ulogic_vector(rd_addr_i'range);
  signal ctrl_wr_en   : std_ulogic;
  signal ctrl_wr_data : std_ulogic_vector(63 downto 0);
  signal ctrl_wr_done : std_ulogic;

  signal ctrl_rd_addr    : std_ulogic_vector(rd_addr_i'range);
  signal ctrl_rd_en      : std_ulogic;
  signal ctrl_rd_data    : std_ulogic_vector(63 downto 0);
  signal ctrl_rd_data_en : std_ulogic;

  signal ctrl_bus_rst   : std_ulogic;
  signal ctrl_bit_send  : std_ulogic;
  signal ctrl_bit_tx    : std_ulogic;
  signal ctrl_bit_recv  : std_ulogic;
  signal ctrl_bit_rx    : std_ulogic;
  signal ctrl_bit_rx_en : std_ulogic;
  signal ctrl_bit_done  : std_ulogic;

  signal ctrl_busy : std_ulogic;

  --! @}

begin -- architecture rtl

  -----------------------------------------------------------------------------
  -- Outputs
  -----------------------------------------------------------------------------

  -- The 1-wire transceiver circuit might invert the signal level
  tx_o <= (not tx) when invert_bus_g else tx;

  strong_pullup_o <= (not strong_pullup) when invert_pullup_g else strong_pullup;

  busy_o <= ctrl_busy;

  rd_data_o    <= mem_rd_data;
  rd_data_en_o <= mem_rd_data_en and (not ctrl_busy);
  rd_busy_o    <= mem_rd_busy or ctrl_busy;

  -----------------------------------------------------------------------------
  -- Signal Assignments
  -----------------------------------------------------------------------------

  -- The 1-wire transceiver circuit might invert the signal level
  rx <= (not rx_i) when invert_bus_g else rx_i;

  if_bus_rst <= dcvr_bus_rst or ctrl_bus_rst;
  if_send    <= dcvr_bit_send or ctrl_bit_send;
  if_tx_data <= dcvr_bit_tx or ctrl_bit_tx;
  if_recv    <= dcvr_bit_recv or ctrl_bit_recv;

  dcvr_bit_rx    <= if_rx_data;
  dcvr_bit_rx_en <= if_rx_data_en;
  dcvr_bit_done  <= if_done;

  ctrl_bit_rx    <= if_rx_data;
  ctrl_bit_rx_en <= if_rx_data_en;
  ctrl_bit_done  <= if_done;

  mem_wr_addr  <= ctrl_wr_addr;
  mem_wr_en    <= ctrl_wr_en or dcvr_id_en;
  mem_wr_data  <= dcvr_id when dcvr_id_en = '1' else ctrl_wr_data;
  ctrl_wr_done <= mem_wr_done;

  mem_rd_addr <= ctrl_rd_addr when ctrl_rd_en = '1' else rd_addr_i;
  mem_rd_en   <= rd_en_i or ctrl_rd_en;

  ctrl_rd_data    <= mem_rd_data;
  ctrl_rd_data_en <= mem_rd_data_en and ctrl_busy;

  -----------------------------------------------------------------------------
  -- Instantiations
  -----------------------------------------------------------------------------

  interface_inst : entity work.onewire_interface
    generic map (
      clk_frequency_g => clk_frequency_g)
    port map (
      clk_i       => clk_i,
      rst_asy_n_i => rst_asy_n_i,
      rst_syn_i   => rst_syn_i,

      bus_rst_i => if_bus_rst,
      send_i    => if_send,
      data_i    => if_tx_data,
      recv_i    => if_recv,
      data_o    => if_rx_data,
      data_en_o => if_rx_data_en,
      done_o    => if_done,

      rx_i => rx,
      tx_o => tx);

  discover_inst : entity work.onewire_discover
    port map (
      clk_i       => clk_i,
      rst_asy_n_i => rst_asy_n_i,
      rst_syn_i   => rst_syn_i,

      discover_i  => dcvr_start,
      id_o        => dcvr_id,
      id_en_o     => dcvr_id_en,
      done_o      => dcvr_done,

      bus_rst_o  => dcvr_bus_rst,
      bit_send_o => dcvr_bit_send,
      bit_o      => dcvr_bit_tx,
      bit_recv_o => dcvr_bit_recv,
      bit_i      => dcvr_bit_rx,
      bit_en_i   => dcvr_bit_rx_en,
      bit_done_i => dcvr_bit_done);

  ctrl_inst : entity work.onewire_control
    generic map (
      clk_frequency_g => clk_frequency_g,
      max_devices_g   => max_devices_g)
    port map (
      clk_i       => clk_i,
      rst_asy_n_i => rst_asy_n_i,
      rst_syn_i   => rst_syn_i,

      discover_i       => discover_i,
      get_temp_i       => get_temp_i,
      busy_o           => ctrl_busy,
      done_o           => done_o,
      device_count_o   => device_count_o,
      error_too_many_o => error_too_many_o,
      strong_pullup_o  => strong_pullup,

      discover_o => dcvr_start,
      id_en_i    => dcvr_id_en,
      done_i     => dcvr_done,

      mem_wr_addr_o => ctrl_wr_addr,
      mem_wr_en_o   => ctrl_wr_en,
      mem_wr_data_o => ctrl_wr_data,
      mem_wr_done_i => ctrl_wr_done,

      mem_rd_addr_o    => ctrl_rd_addr,
      mem_rd_en_o      => ctrl_rd_en,
      mem_rd_data_i    => ctrl_rd_data,
      mem_rd_data_en_i => ctrl_rd_data_en,

      bus_rst_o  => ctrl_bus_rst,
      bit_send_o => ctrl_bit_send,
      bit_o      => ctrl_bit_tx,
      bit_recv_o => ctrl_bit_recv,
      bit_i      => ctrl_bit_rx,
      bit_en_i   => ctrl_bit_rx_en,
      bit_done_i => ctrl_bit_done);

  ram_inst : entity work.two_port_ram_tmr
    generic map (
      depth_g => 2 * max_devices_g,
      width_g => rd_data_o'length)
    port map (
      clk_i       => clk_i,
      rst_asy_n_i => rst_asy_n_i,
      rst_syn_i   => rst_syn_i,

      wr_addr_i => mem_wr_addr,
      wr_en_i   => mem_wr_en,
      wr_data_i => mem_wr_data,
      wr_done_o => mem_wr_done,
      wr_busy_o => open,

      rd_addr_i    => mem_rd_addr,
      rd_en_i      => mem_rd_en,
      rd_data_o    => mem_rd_data,
      rd_data_en_o => mem_rd_data_en,
      rd_busy_o    => mem_rd_busy);

end architecture rtl;
