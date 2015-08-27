//-----------------------------------------------------------------------------
// File     : tb_top.sv
// Author   : Johannes Walter <johannes@greenshire.io>
// Copyright: LGPL v2.1
// Brief    : 1-wire ID and temperature sensor testbench. (top-level)
//-----------------------------------------------------------------------------

`timescale 1ns / 100ps

`include "onewire_idtemp_if.sv"

module tb_top;

  //-------------------------------------------------------------------------
  // Clock and Reset
  //-------------------------------------------------------------------------

  // DUT clock period
  parameter clock_period_c = 25.0;
  // DUT reset duration
  parameter reset_duration_c = 42;

  logic clk = 1;
  logic rst_n = 0;

  always #(clock_period_c / 2) clk <= ~clk;
  initial #reset_duration_c rst_n <= 1;

  //-------------------------------------------------------------------------
  // Instances
  //-------------------------------------------------------------------------

  // DUT interface
  onewire_idtemp_if dut_if(clk, rst_n);

  // DUT
  onewire_idtemp dut(
    .clk_i(dut_if.clk_i),
    .rst_asy_n_i(dut_if.rst_asy_n_i),
    .rst_syn_i(dut_if.rst_syn_i),
    .discover_i(dut_if.discover_i),
    .get_temp_i(dut_if.get_temp_i),
    .busy_o(dut_if.busy_o),
    .done_o(dut_if.done_o),
    .device_count_o(dut_if.device_count_o),
    .error_too_many_o(dut_if.error_too_many_o),
    .rd_addr_i(dut_if.rd_addr_i),
    .rd_en_i(dut_if.rd_en_i),
    .rd_data_o(dut_if.rd_data_o),
    .rd_data_en_o(dut_if.rd_data_en_o),
    .rd_busy_o(dut_if.rd_busy_o),
    .strong_pullup_o(dut_if.strong_pullup_o),
    .rx_i(dut_if.rx_i),
    .tx_o(dut_if.tx_o));

  //-------------------------------------------------------------------------
  // Stimulus
  //-------------------------------------------------------------------------

  logic [1:0] id = 2'b01;

  initial begin
    dut_if.rst_syn_i = 0;
    dut_if.discover_i = 0;
    dut_if.get_temp_i = 0;
    dut_if.rd_addr_i = 0;
    dut_if.rd_en_i = 0;

    #100;

    dut_if.discover_i <= 1;
    #100;
    dut_if.discover_i <= 0;
    @(posedge dut_if.done_o);

    #100;

    dut_if.get_temp_i <= 1;
    #100;
    dut_if.get_temp_i <= 0;
    @(posedge dut_if.done_o);

    #1000;
    $finish;
  end

  always @(negedge dut_if.tx_o) begin
    dut_if.rx_i <= id[1];
    id <= {id[0], id[1]};
  end

endmodule
