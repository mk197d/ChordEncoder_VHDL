library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_textio.all;
use std.textio.all;
use ieee.numeric_std.all;


entity CHORDEncoder is
    port (
        clk, rst: in std_logic;
        a: in std_logic_vector(7 downto 0);
        data_valid: out std_logic;
        z: out std_logic_vector(7 downto 0)
    );
end entity CHORDEncoder;

architecture behaviour of CHORDEncoder is

    component ring_buffer is
        generic (
          RAM_WIDTH : natural := 8;
          RAM_DEPTH : natural := 33
        );
        port (
          clk : in std_logic;
          rstB : in std_logic;
       
          -- Write port
          wr_en : in std_logic;
          wr_data : in std_logic_vector(RAM_WIDTH - 1 downto 0);
       
          -- Read port
          rd_en : in std_logic;
          rd_valid : out std_logic;
          rd_data : out std_logic_vector(RAM_WIDTH - 1 downto 0);
       
          -- Flags
          empty : out std_logic;
          empty_next : out std_logic;
          full : out std_logic;
          full_next : out std_logic;
       
          -- The number of elements in the FIFO
          fill_count : out integer range RAM_DEPTH - 1 downto 0
        );
      end component;

    signal output_reg: std_logic_vector(7 downto 0) := (others => '0');
    signal isDataModified: std_logic := '0';
    signal isComputationComplete: std_logic := '0';
    signal isInserted: std_logic := '0';

    signal givenData_rd, givenData_wr: std_logic_vector(7 downto 0) := (others => '0'); 
    signal gDwr_en, gDrd_en, gD_rst, gD_clk: std_logic := '0';
    signal gD_empty, gD_enext, gD_full, gD_fnext: std_logic;

    signal modifiedData_rd, modifiedData_wr: std_logic_vector(7 downto 0) := (others => '0');
    signal mDwr_en, mDrd_en, mD_rst, mD_clk: std_logic := '0';
    signal mD_empty, mD_enext, mD_full, mD_fnext: std_logic;

    signal outputList_rd, outputList_wr: std_logic_vector(7 downto 0) := (others => '0');
    signal oLwr_en, oLrd_en, oL_rst, oL_clk: std_logic := '0';
    signal oL_empty, oL_enext, oL_full, oL_fnext: std_logic;


    type arr_type is array (0 to 11) of std_logic_vector(7 downto 0);
    signal cyclicList : arr_type;
    subtype arr_index is integer range arr_type'range;
    signal listPointer, listPointerS: integer;
    signal gD_v, mD_v, oL_v: std_logic;

    signal modCurr, modNext: std_logic_vector(7 downto 0) := (others => '0');
    signal outCurr, outNext1, outNext2, outNext3: std_logic_vector(7 downto 0) := (others => '0');
    signal MJ1, MJ2, MJ7: std_logic_vector(7 downto 0) := (others => '0');
    signal mi1, mi2: std_logic_vector(7 downto 0) := (others => '0');
    signal s1, s2: std_logic_vector(7 downto 0) := (others => '0');
    
begin
    givenData: ring_buffer
    port map(
        rd_data => givenData_rd,
        wr_data => givenData_wr,
        clk => gD_clk,
        rstB => gD_rst,
        wr_en => gDwr_en,
        rd_en => gDrd_en,
        rd_valid => gD_v,
        empty => gD_empty,
        empty_next => gD_enext,
        full => gd_full,
        full_next => gD_fnext
    );
    modifiedData: ring_buffer
    port map(
        rd_data => modifiedData_rd,
        wr_data => modifiedData_wr,
        clk => mD_clk,
        rstB => mD_rst,
        wr_en => mDwr_en,
        rd_en => mDrd_en,
        rd_valid => mD_v,
        empty => mD_empty,
        empty_next => mD_enext,
        full => md_full,
        full_next => mD_fnext
    );
    outputList: ring_buffer
    port map(
        rd_data => outputList_rd,
        wr_data => outputList_wr,
        clk => oL_clk,
        rstB => oL_rst,
        wr_en => oLwr_en,
        rd_en => oLrd_en,
        rd_valid => oL_v,
        empty => oL_empty,
        empty_next => oL_enext,
        full => oL_full,
        full_next => oL_fnext
    );

    cyclicList(0) <= "01000011";
    cyclicList(1) <= "01100100";
    cyclicList(2) <= "01000100";
    cyclicList(3) <= "01100101";
    cyclicList(4) <= "01000101";
    cyclicList(5) <= "01000110";
    cyclicList(6) <= "01100111";
    cyclicList(7) <= "01000111";
    cyclicList(8) <= "01100001";
    cyclicList(9) <= "01000001";
    cyclicList(10) <= "01100010";
    cyclicList(11) <= "01000010";

    PROC_SEQUENCER : process
    begin
        data_valid <= '0';
        z <= "00000000";
        wait for 2 ns;
        while a /= "00000000" loop
            gDwr_en <= '1';
            gDrd_en <= '0';
            mDwr_en <= '0';
            mDrd_en <= '0';
            oLwr_en <= '0';
            oLrd_en <= '0';

            gD_clk <= '1';
            givenData_wr <= a;
            wait for 1 ns;
            gD_clk <= '0';  
            wait for 1 ns;
            report "Taking Input: " & integer'image(to_integer(unsigned(givenData_wr)));
        end loop;
        
        gDwr_en <= '0';
        gDrd_en <= '1';
        mDwr_en <= '0';
        mDrd_en <= '0';
        oLwr_en <= '0';
        oLrd_en <= '0';
        wait for 1 ns;

        if gD_empty = '0' then
            gD_clk <= '1';
            modNext <= givenData_rd;
            wait for 1 ns;
            gD_clk <= '0';
            wait for 1 ns;
            report "modCurr is: " & integer'image(to_integer(unsigned(modCurr)));
            report "modNext is: " & integer'image(to_integer(unsigned(modNext)));
        end if;

        wait for 1 ns;
        while isDataModified = '0' loop
            report "modifying";
            wait for 1 ns;
            gDwr_en <= '0';
            gDrd_en <= '1';
            mDwr_en <= '1';
            mDrd_en <= '0';
            oLwr_en <= '0';
            oLrd_en <= '0';
            wait for 1 ns;

            if gD_empty = '0' then
                gD_clk <= '1';
                modCurr <= modNext;
                wait for 1 ns;
                modNext <= givenData_rd;
                wait for 1 ns;
                report "modCurr is: " & integer'image(to_integer(unsigned(modCurr)));
                report "modNext is: " & integer'image(to_integer(unsigned(modNext)));
                gD_clk <= '0';
                wait for 1 ns;
            end if;

            if modCurr /= "00000000" and modNext /= "00000000" then
                if modNext = "00011111" then
                    mD_clk <= '1';
                    case modCurr is
                        when "01000010" =>                  -- B#
                            modifiedData_wr <= "01000011";
                        when "01000011" =>                  -- C#             
                            modifiedData_wr <= "01100100";
                        when "01000100" =>                  -- D#
                            modifiedData_wr <= "01100101";
                        when "01000110" =>                  -- F#
                            modifiedData_wr <= "01100111";
                        when "01000111" =>                  -- G#
                            modifiedData_wr <= "01100001";
                        when "01000001" =>                  -- A#
                            modifiedData_wr <= "01100010";
                        when "01000101" =>                  -- E#
                            modifiedData_wr <= "01000110"; 
                        when "01100110" =>                  -- f#
                            modifiedData_wr <= "01000110";
                        when others =>
                            modifiedData_wr <= "00000000";
                    end case;
                    wait for 1 ns;
                    report "Inserting1: " & integer'image(to_integer(unsigned(modifiedData_wr)));
                    mD_clk <= '0';
                    wait for 1 ns;
                    if gD_empty = '0' then 
                        gD_clk <= '1';
                        modCurr <= modNext;
                        wait for 1 ns;
                        modNext <= givenData_rd;
                        wait for 1 ns;
                        gD_clk <= '0';
                        report "modCurr is: " & integer'image(to_integer(unsigned(modCurr)));
                        report "modNext is: " & integer'image(to_integer(unsigned(modNext)));
                        wait for 1 ns;
                    else 
                        isDataModified <= '1';
                        wait for 1 ns;
                    end if;
                elsif modNext /= "00000000" and modCurr /= "00000000" then
                    mD_clk <= '1';
                    if modCurr = "01100110" then
                        modifiedData_wr <= "01000101";
                    else
                        modifiedData_wr <= modCurr;
                    end if;
                    wait for 1 ns;
                    report "Inserting2: " & integer'image(to_integer(unsigned(modifiedData_wr)));
                    mD_clk <= '0';
                    wait for 1 ns;
                end if;
                if gD_empty = '1' and isDataModified = '0' then
                    report "modifying 14";

                    mD_clk <= '1';
                    if modNext /= "00011111" then
                        if modNext = "01100110" then
                            modifiedData_wr <= "01000101";
                        else
                            modifiedData_wr <= modNext;
                        end if;
                        wait for 1 ns;
                        mD_clk <= '0';
                        report "Inserting4: " & integer'image(to_integer(unsigned(modifiedData_wr)));
                        isDataModified <= '1';
                        wait for 1 ns;
                    else
                        isDataModified <= '1';
                        wait for 1 ns;
                    end if;
                end if;
            end if;
        end loop;

        while isComputationComplete = '0' loop
            report "computing";
            wait for 1 ns;
            gDwr_en <= '0';
            gDrd_en <= '0';
            mDwr_en <= '0';
            mDrd_en <= '1';
            oLwr_en <= '1';
            oLrd_en <= '0';
            wait for 1 ns;
            
            if mD_empty = '0' then
                mD_clk <= '1';
                outCurr <= outNext1;
                wait for 1 ns;
                outNext1 <= outNext2;
                wait for 1 ns;
                outNext2 <= outNext3;
                wait for 1 ns;
                outNext3 <= modifiedData_rd;
                wait for 1 ns;
                mD_clk <= '0';
                wait for 1 ns;
            end if;
            report "out_Curr: " & integer'image(to_integer(unsigned(outCurr)));
            report "out_Next1: " & integer'image(to_integer(unsigned(outNext1)));
            report "out_Next2: " & integer'image(to_integer(unsigned(outNext2)));
            report "out_Next3: " & integer'image(to_integer(unsigned(outNext3)));


            if outNext2 /= "00000000" and outNext1 /= "00000000" and outCurr /= "00000000" and outNext3 /= "00000000" then
                listPointer <= 0;
                wait for 1 ns;
                while cyclicList(listPointer) /= outCurr loop
                    listPointer <= listPointer + 1;
                    wait for 1 ns;
                end loop;
                listPointerS <= listPointer;
                wait for 1 ns;

                -- checking Major
                listPointer <= (listPointer + 4) mod 12;
                wait for 1 ns;
                MJ1 <= cyclicList(listPointer);
                wait for 1 ns;
                if outNext1 = MJ1 and isComputationComplete = '0' then
                    listPointer <= (listPointer + 3) mod 12;
                    wait for 1 ns;
                    MJ2 <= cyclicList(listPointer);
                    wait for 1 ns;
                    if outNext2 = MJ2 then
                        listPointer <= (listPointer + 3) mod 12;
                        wait for 1 ns;
                        MJ7 <= cyclicList(listPointer);
                        wait for 1 ns;
                        if MJ7 = outNext3 then
                            oL_clk <= '1';
                            outputList_wr <= outCurr;
                            wait for 1 ns;
                            report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                            oL_clk <= '0';
                            wait for 1 ns;
                            oL_clk <= '1';
                            outputList_wr <= "00110111";
                            wait for 1 ns;
                            report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                            oL_clk <= '0';

                            outCurr <= "00000000";
                            outNext1 <= "00000000";
                            outNext2 <= "00000000";
                            outNext3 <= "00000000";
                            wait for 1 ns;

                        else
                            oL_clk <= '1';
                            outputList_wr <= outCurr;
                            wait for 1 ns;
                            report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                            oL_clk <= '0';
                            wait for 1 ns;
                            oL_clk <= '1';
                            outputList_wr <= "01001101";
                            wait for 1 ns;
                            report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                            oL_clk <= '0';

                            outCurr <= "00000000";
                            outNext1 <= "00000000";
                            outNext2 <= "00000000";
                            wait for 1 ns;
                        end if;
                    end if;
                end if;
                
                listPointer <= listPointerS;
                wait for 1 ns;
                -- checking Minor
                listPointer <= (listPointer + 3) mod 12;
                wait for 1 ns;
                mi1 <= cyclicList(listPointer);
                wait for 1 ns;
                if outNext1 = mi1 and isComputationComplete = '0' then
                    listPointer <= (listPointer + 4) mod 12;
                    wait for 1 ns;
                    mi2 <= cyclicList(listPointer);
                    wait for 1 ns;
                    if outNext2 = mi2 then
                        oL_clk <= '1';
                        outputList_wr <= outCurr;
                        wait for 1 ns;
                        report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                        oL_clk <= '0';
                        wait for 1 ns;
                        oL_clk <= '1';
                        outputList_wr <= "01101101";
                        wait for 1 ns;
                        report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                        oL_clk <= '0';
                        
                        outCurr <= "00000000";
                        outNext1 <= "00000000";
                        outNext2 <= "00000000";
                        wait for 1 ns;
                    end if;
                end if;

                listPointer <= listPointerS;
                wait for 1 ns;
                -- checking suspended Triads
                listPointer <= (listPointer + 5) mod 12;
                wait for 1 ns;
                s1 <= cyclicList(listPointer);
                wait for 1 ns;
                if outNext1 = s1 and isComputationComplete = '0' then
                    listPointer <= (listPointer + 2) mod 12;
                    wait for 1 ns;
                    s2 <= cyclicList(listPointer);
                    wait for 1 ns;
                    if outNext2 = s2 then
                        oL_clk <= '1';
                        outputList_wr <= outCurr;
                        wait for 1 ns;
                        report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                        oL_clk <= '0';
                        wait for 1 ns;
                        oL_clk <= '1';
                        outputList_wr <= "01110011";
                        wait for 1 ns;
                        report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                        oL_clk <= '0';

                        outCurr <= "00000000";
                        outNext1 <= "00000000";
                        outNext2 <= "00000000";
                        wait for 1 ns;
                    end if;
                end if;

                oL_clk <= '1';
                outputList_wr <= outCurr;
                wait for 1 ns;
                report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                oL_clk <= '0';
                outCurr <= "00000000";
                wait for 1 ns;

            elsif mD_empty = '1' then
                if outNext1 /= "00000000" then
                    listPointer <= 0;
                    wait for 1 ns;
                    while cyclicList(listPointer) /= outNext1 loop
                        listPointer <= listPointer + 1;
                        wait for 1 ns;
                    end loop;
                    listPointerS <= listPointer;
                    wait for 1 ns;
                    
                    -- checking Major
                    listPointer <= (listPointer + 4) mod 12;
                    wait for 1 ns;
                    MJ1 <= cyclicList(listPointer);
                    wait for 1 ns;
                    if outNext2 = MJ1 and isComputationComplete = '0' then
                        listPointer <= (listPointer + 3) mod 12;
                        wait for 1 ns;
                        MJ2 <= cyclicList(listPointer);
                        wait for 1 ns;
                        if outNext3 = MJ2 then
                            
                            oL_clk <= '1';
                            outputList_wr <= outNext1;
                            wait for 1 ns;
                            report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                            oL_clk <= '0';
                            wait for 1 ns;
                            oL_clk <= '1';
                            outputList_wr <= "01001101";
                            wait for 1 ns;
                            report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                            oL_clk <= '0';

                            outNext1 <= "00000000";
                            outNext2 <= "00000000";
                            outNext3 <= "00000000";
                            isComputationComplete <= '1';
                            wait for 1 ns;
                        end if;
                    end if;
                    
                    listPointer <= listPointerS;
                    wait for 1 ns;
                    -- checking Minor
                    listPointer <= (listPointer + 3) mod 12;
                    wait for 1 ns;
                    mi1 <= cyclicList(listPointer);
                    wait for 1 ns;
                    if outNext2 = mi1 and isComputationComplete = '0' then
                        listPointer <= (listPointer + 4) mod 12;
                        wait for 1 ns;
                        mi2 <= cyclicList(listPointer);
                        wait for 1 ns;
                        if outNext3 = mi2 then
                            oL_clk <= '1';
                            outputList_wr <= outNext1;
                            wait for 1 ns;
                            report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                            oL_clk <= '0';
                            wait for 1 ns;
                            oL_clk <= '1';
                            outputList_wr <= "01101101";
                            wait for 1 ns;
                            report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                            oL_clk <= '0';
                            
                            outNext1 <= "00000000";
                            outNext2 <= "00000000";
                            outNext3 <= "00000000";
                            isComputationComplete <= '1';
                            wait for 1 ns;
                        end if;
                    end if;

                    listPointer <= listPointerS;
                    wait for 1 ns;
                    -- checking suspended Triads
                    listPointer <= (listPointer + 5) mod 12;
                    wait for 1 ns;
                    s1 <= cyclicList(listPointer);
                    wait for 1 ns;
                    if outNext2 = s1 and isComputationComplete = '0' then
                        listPointer <= (listPointer + 2) mod 12;
                        wait for 1 ns;
                        s2 <= cyclicList(listPointer);
                        wait for 1 ns;
                        if outNext3 = s2 then
                            oL_clk <= '1';
                            outputList_wr <= outNext1;
                            wait for 1 ns;
                            report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                            oL_clk <= '0';
                            wait for 1 ns;
                            oL_clk <= '1';
                            outputList_wr <= "01110011";
                            wait for 1 ns;
                            report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                            oL_clk <= '0';

                            outNext1 <= "00000000";
                            outNext2 <= "00000000";
                            outNext3 <= "00000000";
                            isComputationComplete <= '1';
                            wait for 1 ns;
                        end if;
                    end if;
                    oL_clk <= '1';
                    outputList_wr <= outNext1;
                    wait for 1 ns;
                    report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                    oL_clk <= '0';
                    outNext1 <= "00000000";
                    wait for 1 ns;
                else
                    oL_clk <= '1';
                    outputList_wr <= outNext2;
                    wait for 1 ns;
                    report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                    oL_clk <= '0';
                    wait for 1 ns;
                    oL_clk <= '1';
                    outputList_wr <= outNext3;
                    wait for 1 ns;
                    report "outputIN: " & integer'image(to_integer(unsigned(outputList_wr)));
                    oL_clk <= '0';

                    outNext2 <= "00000000";
                    outNext3 <= "00000000";
                    isComputationComplete <= '1';
                    wait for 1 ns;
                end if;
            end if;
        end loop;

        while isInserted = '0' loop
            report "inserting";
            gDwr_en <= '0';
            gDrd_en <= '0';
            mDwr_en <= '0';
            mDrd_en <= '0';
            oLwr_en <= '0';
            oLrd_en <= '1';
            wait for 1 ns;

            oL_clk <= '1';
            wait for 1 ns;
            oL_clk <= '0';
            wait for 1 ns;

            while oL_empty = '0' loop
                oL_clk <= '1';
                z <= outputList_rd;
                output_reg <= outputList_rd;
                wait for 1 ns;
                report "Giving Output: " & integer'image(to_integer(unsigned(output_reg)));
                oL_clk <= '0';
                wait for 1 ns;
            end loop;
            z <= outputList_rd;
            output_reg <= outputList_rd;
            wait for 1 ns;
            report "Giving Output: " & integer'image(to_integer(unsigned(output_reg)));
            data_valid <= '1';
            isInserted <= '1';
            output_reg <= "00000000";
            wait for 1 ns;
        end loop;

    end process;
end architecture;
