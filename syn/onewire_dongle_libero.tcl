new_project \
  -location {c:/build/onewire_dongle} \
  -name {onewire_dongle} \
  -project_description {} \
  -block_mode 0 \
  -hdl {VHDL} \
  -family {ProASIC3} \
  -die {A3P400} \
  -package {208 PQFP} \
  -speed {STD} \
  -die_voltage {1.5} \
  -adv_options {IO_DEFT_STD:LVTTL} \
  -adv_options {RESTRICTPROBEPINS:1} \
  -adv_options {TEMPR:COM} \
  -adv_options {VCCI_1.5_VOLTR:COM} \
  -adv_options {VCCI_1.8_VOLTR:COM} \
  -adv_options {VCCI_2.5_VOLTR:COM} \
  -adv_options {VCCI_3.3_VOLTR:COM} \
  -adv_options {VOLTR:COM}

create_links \
  -sdc {./onewire_dongle.sdc} \
  -sdc {./onewire_dongle_syn.sdc} \
  -pdc {./onewire_dongle.pdc}

create_links \
  -hdl_source {../src/rtl/onewire_dongle_top.vhd} \
  -hdl_source {../src/rtl/onewire_idtemp.vhd} \
  -hdl_source {../src/rtl/onewire_idtemp_pkg.vhd} \
  -hdl_source {../src/rtl/onewire_idtemp/onewire_control.vhd} \
  -hdl_source {../src/rtl/onewire_idtemp/onewire_crc.vhd} \
  -hdl_source {../src/rtl/onewire_idtemp/onewire_interface.vhd} \
  -hdl_source {../src/rtl/onewire_idtemp/onewire_discover.vhd}

create_links \
  -hdl_source {../../../../common/vhd/communication/uart/src/rtl/uart_tx.vhd} \
  -hdl_source {../../../../common/vhd/communication/uart/src/rtl/uart_rx.vhd} \
  -hdl_source {../../../../common/vhd/generic/external_inputs/src/rtl/external_inputs.vhd} \
  -hdl_source {../../../../common/vhd/generic/edge_detector/src/rtl/edge_detector.vhd} \
  -hdl_source {../../../../common/vhd/generic/bit_clock_recovery/src/rtl/bit_clock_recovery.vhd} \
  -hdl_source {../../../../common/vhd/generic/array_transmitter/src/rtl/array_tx.vhd} \
  -hdl_source {../../../../common/vhd/generic/reset_generator/src/rtl/reset_generator.vhd} \
  -hdl_source {../../../../common/vhd/generic/delay/src/rtl/delay.vhd} \
  -hdl_source {../../../../common/vhd/generic/glitch_filter/src/rtl/glitch_filter.vhd} \
  -hdl_source {../../../../common/vhd/generic/lfsr_strobe_generator/src/rtl/lfsr_strobe_generator.vhd} \
  -hdl_source {../../../../common/vhd/packages/lfsr/src/rtl/lfsr_pkg.vhd} \
  -hdl_source {../../../../common/vhd/generic/mem_data_triplicator/src/rtl/mem_data_triplicator/mem_data_triplicator_addr.vhd} \
  -hdl_source {../../../../common/vhd/generic/mem_data_triplicator/src/rtl/mem_data_triplicator/mem_data_triplicator_rd.vhd} \
  -hdl_source {../../../../common/vhd/generic/mem_data_triplicator/src/rtl/mem_data_triplicator/mem_data_triplicator_wr.vhd} \
  -hdl_source {../../../../common/vhd/generic/mem_data_triplicator/src/rtl/mem_data_triplicator_rd_only.vhd} \
  -hdl_source {../../../../common/vhd/generic/mem_data_triplicator/src/rtl/mem_data_triplicator_wr_only.vhd} \
  -hdl_source {../../../../common/vhd/memory/two_port_ram/src/rtl/two_port_ram.vhd} \
  -hdl_source {../../../../common/vhd/memory/two_port_ram/src/rtl/two_port_ram_tmr.vhd}

set_root -module {onewire_dongle_top::work}

organize_tool_files \
  -tool {SYNTHESIZE} \
  -file {./onewire_dongle_syn.sdc} \
  -module {onewire_dongle_top::work} \
  -input_type {constraint}

organize_tool_files \
  -tool {COMPILE} \
  -file {./onewire_dongle.sdc} \
  -file {./onewire_dongle.pdc} \
  -module {onewire_dongle_top::work} \
  -input_type {constraint}

save_project
