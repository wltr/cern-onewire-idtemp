#
# Synthesis Parameters
#

# Clock
define_clock {clk_i} -name {clk_i} -period 25.0

# Enable TMR
#define_attribute {v:work.onewire_dongle_top} syn_radhardlevel {tmr}

# FSM encoding
define_attribute {i:*.reg\.state[*]} syn_encoding {safe, onehot}
define_attribute {i:*.*.reg\.state[*]} syn_encoding {safe, onehot}
define_attribute {i:*.*.*.reg\.state[*]} syn_encoding {safe, onehot}
define_attribute {i:*.*.*.*.reg\.state[*]} syn_encoding {safe, onehot}
define_attribute {i:*.*.*.*.*.reg\.state[*]} syn_encoding {safe, onehot}
define_attribute {i:*.*.*.*.*.*.reg\.state[*]} syn_encoding {safe, onehot}
