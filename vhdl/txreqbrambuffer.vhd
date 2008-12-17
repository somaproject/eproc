library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

library UNISIM;
use UNISIM.VComponents.all;

library SOMA;
use SOMA.somabackplane.all;
use soma.somabackplane;


entity txreqbrambuffer is
  port (
    -- input side
    CLK       : in  std_logic;
    SRC       : in  std_logic_vector(7 downto 0);
    ADDRIN    : in  std_logic_vector(2 downto 0);
    WEIN      : in  std_logic;
    DIN       : in  std_logic_vector(15 downto 0);
    -- outputs
    OUTCLK    : in  std_logic;
    SENDREQ   : out std_logic;
    SENDGRANT : in  std_logic;
    SENDDONE  : out std_logic;
    DOUT      : out std_logic_vector(7 downto 0));
end txreqbrambuffer;

architecture Behavioral of txreqbrambuffer is

  -- input signals
  signal addrinl : std_logic_vector(2 downto 0)  := (others => '0');
  signal dinl    : std_logic_vector(15 downto 0) := (others => '0');
  signal weinl , weinll  : std_logic                     := '0';

  signal decode : std_logic_vector(15 downto 0) := (others => '0');

  signal dib , dob   : std_logic_vector(15 downto 0) := (others => '0');
  signal addrb  : std_logic_vector(9 downto 0)  := (others => '0');
  signal addrbl : std_logic_vector(9 downto 4)  := (others => '0');
  signal web    : std_logic                     := '0';


  -- oputput side
  signal doa   : std_logic_vector(7 downto 0)  := (others => '0');
  signal addra : std_logic_vector(10 downto 0) := (others => '0');

  signal addrbll  : std_logic_vector(9 downto 4) := (others => '0');
  signal incaddra : std_logic                    := '0';

  signal ena : std_logic := '0';
  signal wea : std_logic := '0';

  type states is (none, getbcasth, getbcastl, armdata, reqs, datas, dones);
  signal cs, ns : states := none;

  signal bcast : std_logic := '0';

begin  -- Behavioral

  SENDREQ  <= '1' when cs = reqs  else '0';
  SENDDONE <= '1' when cs = dones else '0';

  buffer_inst : RAMB16_S9_S18
    generic map (
      SIM_COLLISION_CHECK => "NONE",
      WRITE_MODE_A          => "READ_FIRST")
    port map (
      DOA                 => doa,
      DOB                 => dob, 
      ADDRA               => addra,
      ADDRB               => addrb,
      CLKA                => OUTCLK,
      CLKB                => clk,
      DIA                 => X"00",
      DIB                 => dib,
      ENA                 => ena,
      ENB                 => '1',
      SSRA                => '0',
      SSRB                => '0',
      DIPA                => "0",
      DIPB                => "00",
      WEA                 => wea,
      WEB                 => web);

  web <= weinll when addrinl = "111" else
         weinl; 


  addrb(3 downto 0) <= X"6"                         when addrinl = "000" else
                       X"7"                         when addrinl = "001" else
                       X"8"                         when addrinl = "010" else
                       X"9"                         when addrinl = "011" else
                       X"A"                         when addrinl = "100" else
                       X"B"                         when addrinl = "101" else
                       X"0"                         when addrinl = "110" else
                       ('0' & dinl(6 downto 4)) + 1; 

  dib <= SRC & dinl(7 downto 0) when addrinl = "000" else
         decode  or dob         when addrinl = "111" else
         X"0001"                when addrinl = "110" else
         dinl(7 downto 0) & dinl(15 downto 8);

  decode <= X"0001" when dinl(3 downto 0) = X"0" else
            X"0002" when dinl(3 downto 0) = X"1" else
            X"0004" when dinl(3 downto 0) = X"2" else
            X"0008" when dinl(3 downto 0) = X"3" else
            X"0010" when dinl(3 downto 0) = X"4" else
            X"0020" when dinl(3 downto 0) = X"5" else
            X"0040" when dinl(3 downto 0) = X"6" else
            X"0080" when dinl(3 downto 0) = X"7" else
            X"0100" when dinl(3 downto 0) = X"8" else
            X"0200" when dinl(3 downto 0) = X"9" else
            X"0400" when dinl(3 downto 0) = X"A" else
            X"0800" when dinl(3 downto 0) = X"B" else
            X"1000" when dinl(3 downto 0) = X"C" else
            X"2000" when dinl(3 downto 0) = X"D" else
            X"4000" when dinl(3 downto 0) = X"E" else
            X"8000";

  inputmain : process(CLK)
  begin
    if rising_edge(CLK) then

      addrinl <= ADDRIN;
      dinl    <= DIN;
      weinl   <= WEIN;
      weinll    <= weinl;
      
      if addrinl = "000" and weinl = '1' then
        addrb(9 downto 4) <= addrb(9 downto 4) + 1;
      end if;

      addrbl <= addrb(9 downto 4);

    end if;
  end process inputmain;


  outputmain : process(OUTCLK)
  begin
    if rising_edge(OUTCLK) then
      cs <= ns;

      addrbll <= addrbl;

      if cs = dones then
        addra(10 downto 5) <= addra(10 downto 5) + 1;
      end if;

      if cs = none then
        addra(4 downto 0)   <= (others => '0');
      else
        if incaddra = '1' then
          addra(4 downto 0) <= addra(4 downto 0) + 1;
        end if;
      end if;

      if cs = getbcastl then
        bcast <= doa(0);
      end if;

    end if;
  end process outputmain;

  DOUT <= X"FF" when bcast = '1' and addra(4 downto 0) < "01101" else
          doa;

  fsm : process(cs, addra, addrb, SENDGRANT)
  begin
    case cs is
      when none =>
        incaddra <= '0';
        ena      <= '0';
        wea      <= '0';

        if addra(10 downto 5) /= addrb(9 downto 4) then
          ns <= getbcasth;
        else
          ns <= none;
        end if;

      when getbcasth =>
        incaddra <= '1';
        ena      <= '1';
        wea      <= '1';
        ns       <= getbcastl;

      when getbcastl =>
        incaddra <= '1';
        ena      <= '1';
        wea      <= '1';
        ns       <= armdata;

      when armdata =>
        incaddra <= '1';
        ena      <= '1';
        wea      <= '1';
        ns       <= reqs;

      when reqs =>
        incaddra <= '0';
        ena      <= '0';
        wea      <= '0';
        if SENDGRANT = '1' then
          ns     <= datas;
        else
          ns     <= reqs;
        end if;

      when datas =>
        incaddra <= '1';
        ena      <= '1';
        wea      <= '1';
        if addra(4 downto 0) = "11000" then
          ns     <= dones;
        else
          ns     <= datas;
        end if;

      when dones =>
        incaddra <= '0';
        ena      <= '0';
        wea      <= '0';
        ns       <= none;

      when others =>
        incaddra <= '0';
        ena      <= '0';
        wea      <= '0';
        ns       <= none;

    end case;

  end process fsm;


end Behavioral;
