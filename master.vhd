--------------------------------------------------------------------------------
--
--   FileName:         lcd_example.vhd
--   Dependencies:     none
--   Design Software:  Quartus II 32-bit Version 11.1 Build 173 SJ Full Version
--
--   HDL CODE IS PROVIDED "AS IS."  DIGI-KEY EXPRESSLY DISCLAIMS ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL DIGI-KEY
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 6/13/2012 Scott Larson
--     Initial Public Release
--
--   Prints "123456789" on a HD44780 compatible 8-bit interface character LCD 
--   module using the lcd_controller.vhd component.
--
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

ENTITY master IS
  PORT(
      KEY 		: IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      CLOCK_50  : IN  STD_LOGIC;  --system clock
      LCD_ON    : OUT STD_LOGIC;  --turn on DE2 lcd
      LCD_RW, LCD_RS, LCD_EN : OUT STD_LOGIC;  --read/write, setup/data, and enable for lcd
      LEDG 		: OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      LCD_DATA  : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      PS2_KBCLK	: IN std_logic;
      PS2_KBDAT   : IN std_logic;
      LEDR		: OUT std_logic_vector(15 downto 0);
      HEX6		: OUT std_logic_vector(6 downto 0);
      HEX7		: OUT std_logic_vector(6 downto 0)); --data signals for lcd
END master;

ARCHITECTURE behavior OF master IS
  SIGNAL   lcd_enable : STD_LOGIC;
  SIGNAL   lcd_bus    : STD_LOGIC_VECTOR(9 DOWNTO 0);
  SIGNAL   lcd_busy   : STD_LOGIC;
  SIGNAL   K2         : STD_LOGIC;
  SIGNAL   K3		  : STD_LOGIC;
  SIGNAL FLAG : STD_LOGIC;
  type rom_pattern_array is array(1 to 16) of std_logic_vector(7 downto 0);
  signal line1 : rom_pattern_array;
  signal te	: std_logic_vector(7 downto 0) := "00110000";
   
  
  
  signal asciiN: STD_LOGIC;
  signal ascii : STD_LOGIC_VECTOR(7 DOWNTO 0);
  
  signal addFlag : std_logic;
  signal addFlag1 : std_logic;
  signal A : std_logic_vector(15 downto 0);
  signal B : std_logic_vector(15 downto 0);
  signal ANS : std_logic_vector(15 downto 0);
  
  COMPONENT lcd_controller IS
    PORT(
       clk        : IN  STD_LOGIC; --system clock
       reset_n    : IN  STD_LOGIC; --active low reinitializes lcd
       lcd_enable : IN  STD_LOGIC; --latches data into lcd controller
       lcd_bus    : IN  STD_LOGIC_VECTOR(9 DOWNTO 0); --data and control signals
       busy       : OUT STD_LOGIC; --lcd controller busy/idle feedback
       rw, rs, e  : OUT STD_LOGIC; --read/write, setup/data, and enable for lcd
       lcd_data   : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)); --data signals for lcd
  END COMPONENT;
  component ps2_keyboard_to_ascii is
	GENERIC(
	  clk_freq                  : INTEGER := 50_000_000; --system clock frequency in Hz
	  ps2_debounce_counter_size : INTEGER := 8);  
	PORT(
	  clk        : IN  STD_LOGIC;                     --system clock input
	  ps2_clk    : IN  STD_LOGIC;                     --clock signal from PS2 keyboard
	  ps2_data   : IN  STD_LOGIC;                     --data signal from PS2 keyboard
	  ascii_new  : OUT STD_LOGIC;                     --output flag indicating new ASCII value
	  ascii_code : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)); --ASCII value
end component;
BEGIN
  K2 <= KEY(2);
  K3 <= KEY(3);
  LCD_ON <= '1';
  --instantiate the lcd controller
  dut: lcd_controller
    PORT MAP(clk => CLOCK_50, reset_n => '1', lcd_enable => lcd_enable, lcd_bus => lcd_bus, 
             busy => lcd_busy, rw => LCD_RW, rs => LCD_RS, e => LCD_EN, lcd_data => LCD_DATA);
             
  ps2: ps2_keyboard_to_ascii port map(CLOCK_50, PS2_KBCLK, PS2_KBDAT, asciiN, ascii(6 downto 0));
  
  PROCESS(CLOCK_50)
    VARIABLE char  :  INTEGER RANGE 0 TO 34 := 0;
  BEGIN
    IF(CLOCK_50'EVENT AND CLOCK_50 = '1') THEN
		  IF(lcd_busy = '0' AND lcd_enable = '0') THEN
			lcd_enable <= '1';
			IF(char < 23) THEN
			  char := char + 1;
			END IF;
			CASE char IS
			  WHEN 1 => lcd_bus <= "0010000000";
			  WHEN 2 => lcd_bus <= "10" & line1(char-1);
			  WHEN 3 => lcd_bus <= "10" & line1(char-1);
			  WHEN 4 => lcd_bus <= "10" & line1(char-1);
			  WHEN 5 => lcd_bus <= "10" & line1(char-1);
			  WHEN 6 => lcd_bus <= "10" & line1(char-1);
			  WHEN 7 => lcd_bus <= "10" & line1(char-1);
			  WHEN 8 => lcd_bus <= "10" & line1(char-1);
			  WHEN 9 => lcd_bus <= "10" & line1(char-1);
			  WHEN 10 => lcd_bus <= "10" & line1(char-1);
			  WHEN 11 => lcd_bus <= "10" & line1(char-1);
			  WHEN 12 => lcd_bus <= "10" & line1(char-1);
			  WHEN 13 => lcd_bus <= "10" & line1(char-1);
			  WHEN 14 => lcd_bus <= "10" & line1(char-1);
			  WHEN 15 => lcd_bus <= "10" & line1(char-1);
			  WHEN 16 => lcd_bus <= "10" & line1(char-1);
			  WHEN 17 => lcd_bus <= "10" & line1(char-1);
			  when 18 => lcd_bus <= "0011000000";
			  when 19 => lcd_bus <= "1001010100";
			  when 20 => lcd_bus <= "1000111010";
			  when 21 => lcd_bus <= "10"&te;
			  
			 -- WHEN 18 => lcd_bus <= "0011000000";
			 -- WHEN 19 => lcd_bus <= "10" & x"3" & ANS(3 downto 0);
			  WHEN OTHERS => lcd_enable <= '0'; 
							 char := 0;
			END CASE;
		  ELSE
			lcd_enable <= '0';
		  END IF;
    END IF;
  END PROCESS;
  
	process(ascii, K3, asciiN)
			VARIABLE index  :  INTEGER RANGE 1 TO 16 := 1;
		begin
			if K3 = '0' then
				line1 <= (x"20", x"20", x"20", x"20", x"20", x"20", x"20", x"20", x"20", x"20", x"20", x"20", x"20", x"20", x"20", x"20");
				index := 1;
				addFlag <= '0';
				addFlag1 <= '0';
				A <= x"0000";
				B <= x"0000";
				te <= "00110000";
			elsif asciiN'event and asciiN = '0' then
				if ascii = x"0D" then
					te <= te + '1';
				  if te = "00111001" then
                   te <= te + "01000001" - "00111001";
                   end if;
				else
					line1(index) <= ascii;
					index := index + 1;
				end if;
				
			end if;
		end process;
	
	process(addFlag, K3)
	begin
		if K3 = '0' then
			ANS <= x"0000";
		elsif addFlag = '1' then
			ANS <= A + B;
		end if;
	end process;
	
	
  LEDG(0) <= lcd_enable;
  LEDG(1) <= FLAG;
  LEDG(2) <= addFlag;
  LEDG(3) <= addFlag1;
END behavior;
