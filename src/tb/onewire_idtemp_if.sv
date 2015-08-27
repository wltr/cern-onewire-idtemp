//-----------------------------------------------------------------------------
// File     : onewire_idtemp_if.sv
// Author   : Johannes Walter <johannes@greenshire.io>
// Copyright: LGPL v2.1
// Brief    : 1-wire ID and temperature sensor DUT interface.
//-----------------------------------------------------------------------------

`ifndef ONEWIRE_IDTEMP_IF
`define ONEWIRE_IDTEMP_IF

interface onewire_idtemp_if (input logic clk_i, input logic rst_asy_n_i);

  //-------------------------------------------------------------------------
  // Signals
  //-------------------------------------------------------------------------

  logic rst_syn_i;

  logic        discover_i;
  logic        get_temp_i;
  logic        busy_o;
  logic        done_o;
  logic [4:0]  device_count_o;
  logic        error_too_many_o;
  logic [4:0]  rd_addr_i;
  logic        rd_en_i;
  logic [63:0] rd_data_o;
  logic        rd_data_en_o;
  logic        rd_busy_o;
  logic        strong_pullup_o;
  logic        rx_i;
  logic        tx_o;

  //-------------------------------------------------------------------------
  // Clocking Blocks
  //-------------------------------------------------------------------------

  clocking tb @(posedge clk_i);
    output rst_syn_i;
    output discover_i;
    output get_temp_i;
    input  busy_o;
    input  done_o;
    input  device_count_o;
    input  error_too_many_o;
    output rd_addr_i;
    output rd_en_i;
    input  rd_data_o;
    input  rd_data_en_o;
    input  rd_busy_o;
    input  strong_pullup_o;
    output rx_i;
    input  tx_o;
  endclocking

endinterface

`endif
