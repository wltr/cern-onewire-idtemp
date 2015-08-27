-------------------------------------------------------------------------------
--! @file      onewire_idtemp_pkg.vhd
--! @author    Johannes Walter <johannes@greenshire.io>
--! @copyright LGPL v2.1
--! @brief     Constants for the 1-wire ID and temperature sensor interface.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

--! @brief Package declaration of onewire_idtemp_pkg
package onewire_idtemp_pkg is

  -----------------------------------------------------------------------------
  -- Types and Constants
  -----------------------------------------------------------------------------

  --! DS18B20 ID and temperature sensor family code
  constant code_ds18b20_c : std_ulogic_vector(7 downto 0) := x"28";
  --! DS2401 ID family code
  constant code_ds2401_c  : std_ulogic_vector(7 downto 0) := x"01";

  --! Search command
  constant cmd_search_c  : std_ulogic_vector(7 downto 0) := x"F0";
  --! Match ROM command
  constant cmd_match_c   : std_ulogic_vector(7 downto 0) := x"55";
  --! Skip ROM command
  constant cmd_skip_c    : std_ulogic_vector(7 downto 0) := x"CC";
  --! Convert command
  constant cmd_convert_c : std_ulogic_vector(7 downto 0) := x"44";
  --! Read scratchpad
  constant cmd_read_sp_c : std_ulogic_vector(7 downto 0) := x"BE";

end package onewire_idtemp_pkg;
