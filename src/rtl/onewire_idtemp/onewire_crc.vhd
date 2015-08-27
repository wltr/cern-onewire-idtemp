-------------------------------------------------------------------------------
--! @file      onewire_crc.vhd
--! @author    Johannes Walter <johannes@greenshire.io>
--! @copyright LGPL v2.1
--! @brief     Calculate the CRC of incoming data bits. (x^8 + x^5 + x^4 + 1)
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

--! @brief Entity declaration of onewire_crc
entity onewire_crc is
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

    --! Reset CRC generation
    reset_i   : in  std_ulogic;
    --! Incoming data bit
    data_i    : in  std_ulogic;
    --! Incoming data bit enable
    data_en_i : in  std_ulogic;
    --! CRC valid flag
    valid_o   : out std_ulogic);

    --! @}
end entity onewire_crc;

--! RTL implementation of onewire_crc
architecture rtl of onewire_crc is

  -----------------------------------------------------------------------------
  --! @name Internal Registers
  -----------------------------------------------------------------------------
  --! @{

  signal crc      : std_ulogic_vector(7 downto 0);
  signal pristine : std_ulogic;

  --! @}

begin -- architecture rtl

  -----------------------------------------------------------------------------
  -- Outputs
  -----------------------------------------------------------------------------

  valid_o <= '1' when crc = x"00" and pristine = '0' else '0';

  -----------------------------------------------------------------------------
  -- Registers
  -----------------------------------------------------------------------------

  regs : process (clk_i, rst_asy_n_i) is
    procedure reset is
    begin
      crc      <= (others => '0');
      pristine <= '1';
    end procedure reset;
  begin -- process regs
    if rst_asy_n_i = '0' then
      reset;
    elsif rising_edge(clk_i) then
      if rst_syn_i = '1' or reset_i = '1' then
        reset;
      elsif data_en_i = '1' then
        crc(0) <= crc(1);
        crc(1) <= crc(2);
        crc(2) <= crc(3) xor crc(0) xor data_i;
        crc(3) <= crc(4) xor crc(0) xor data_i;
        crc(4) <= crc(5);
        crc(5) <= crc(6);
        crc(6) <= crc(7);
        crc(7) <= crc(0) xor data_i;

        pristine <= '0';
      end if;
    end if;
  end process regs;

end architecture rtl;
