
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


with Ada.Numerics.Discrete_Random; 

procedure My_Hello_World is

   type Ship is tagged record
      Length : Natural;
      Hits : Natural := 0;
      Name: Unbounded_String;
      Lookup_Index : Integer;
   end record;

   type Display_Grid is array (1 .. 10, 1 .. 10) of Integer;

   subtype Random_Range is Integer range 1 .. 10;
   package Random_Roll is new
     Ada.Numerics.Discrete_Random (Random_Range);
   use Random_Roll;

   subtype Random_Orientation is Integer range 1 .. 2;
   package Random_Orientation_Roll is new
     Ada.Numerics.Discrete_Random (Random_Orientation);
   use Random_Orientation_Roll;

   Number_Generator : Random_Roll.Generator;
   Orientation_Generator : Random_Orientation_Roll.Generator;

   Player_Total_Hits : Integer := 0;
   Opponent_Total_Hits : Integer := 0;

   begin

      declare 

         procedure Place_Ship (
            S : in out Ship; 
            G : in out Display_Grid; 
            Direction : String
            ) is
         begin

            declare
               Coord_X : Integer;
               Coord_Y : Integer;
               Valid_Placement : Boolean;
               Ship_Placed : Boolean := False;
            begin

               while not Ship_Placed loop
                  Valid_Placement := True;

                  Coord_X := Random(Number_Generator);
                  Coord_Y := Random(Number_Generator);

                  if Direction = "horizontal" then

                     if Coord_X + S.Length > 10 then 
                        Valid_Placement := False; -- ship too long
                     end if;

                     for I in 1 .. S.Length loop
                        if Valid_Placement and then G(Coord_X + I, Coord_Y) > 0 then
                           Valid_Placement := False; -- another ship is already there
                        end if;
                     end loop;

                     if Valid_Placement then
                        for I in 1 .. S.Length loop
                           Put_Line(Integer'Image(S.Lookup_Index));
                           G(Coord_X + I, Coord_Y) := S.Lookup_Index;
                           Put_Line (To_String(S.Name));
                           Put_Line (Integer'Image(G(Coord_X + I, Coord_Y)));
                        end loop;
                        Ship_Placed := True;
                     end if;

                  elsif Direction = "vertical" then

                     if Coord_Y + S.Length > 10 then
                        Valid_Placement := False;
                     end if;

                     for I in 1 .. S.Length loop
                        if Valid_Placement and then G(Coord_X, Coord_Y + I) > 0 then
                           Valid_Placement := False;
                        end if;
                     end loop;

                     if Valid_Placement then
                        for I in 1 .. S.Length loop
                           G(Coord_X, Coord_Y + I) := S.Lookup_Index;
                        end loop;
                        Ship_Placed := True;
                     end if;

                  else
                     Ada.Text_IO.Put_Line("Error: Invalid direction");
                     return;
                  end if;

               end loop;

            end;

         end Place_Ship;



         --  Player1_Grid : Grid := (others => (others => 0));
         --  Player2_Grid : Grid := (others => (others => 0));
         Opponent_Grid: Display_Grid := (others => (others => 0));
         Opponent_Display_Grid: Display_Grid := (others => (others => 0));
         --  Opponent_Memory : Grid := (others => (others => 0));

         --  Player_Grid: Grid := (others => (others => 0));

         Player_Grid : Display_Grid := (others => (others => 0));
         Player_Display_Grid: Display_Grid:= (others => (others => 0));

         Turn : Integer;

         type Ship_Info is record
            Name : Unbounded_String;
            Length : Integer;
            Lookup_Index : Integer;
         end record;

         Ship_Template : array (1 .. 5) of Ship_Info := (
            (To_Unbounded_String("Carrier"), 5, 2),
            (To_Unbounded_String("Battleship"), 4, 3),
            (To_Unbounded_String("Cruiser"), 3, 4),
            (To_Unbounded_String("Submarine"), 3, 5),
            (To_Unbounded_String("Destroyer"), 2, 6)
         );

         Player_Ships :  array (1 .. 5) of Ship;
         Opponent_Ships :  array (1 .. 5) of Ship; 


         Water_Symbol : String := "~";
         Ship_Symbol : String := "X";
         Miss_Symbol : String := "O";

         X_Offset : Integer;

         All_Ships_Placed : Boolean := False;

      begin

         Reset(Number_Generator);
         Reset(Orientation_Generator);

         for I in 1 .. 5 loop
            Player_Ships(I).Length := Ship_Template(I).Length;
            Player_Ships(I).Name := Ship_Template(I).Name;
            Player_Ships(I).Lookup_Index := Ship_Template(I).Lookup_Index;

            if Random_Orientation_Roll.Random(Orientation_Generator) = 1 then
               Place_Ship(Player_Ships(I), Player_Grid, "horizontal");
            else
               Place_Ship(Player_Ships(I), Player_Grid, "vertical");
            end if;
         end loop;

         for I in 1 .. 5 loop
            Opponent_Ships(I).Length := Ship_Template(I).Length;
            Opponent_Ships(I).Name := Ship_Template(I).Name;
            Opponent_Ships(I).Lookup_Index := Ship_Template(I).Lookup_Index;

            if Random_Orientation_Roll.Random(Orientation_Generator) = 1 then
               Place_Ship(Opponent_Ships(I), Opponent_Grid, "horizontal");
            else
               Place_Ship(Opponent_Ships(I), Opponent_Grid, "vertical");
            end if;
         end loop;


        
         while True loop
            New_Line;
            Put ("   Opponent Grid            Player Grid");
            New_Line;
   
            for I in Player_Grid'Range loop
               --  Put (Player_Grid(I, 2)'Image);

                  X_Offset := I - 1;

                  Put (X_Offset'Image);
                  Put (" ");

               for J in Opponent_Display_Grid'Range loop
                  if Opponent_Display_Grid(I, J) = 0 then
                     Put (Water_Symbol);
                  else if Opponent_Display_Grid(I, J) = 1 then
                     Put (Miss_Symbol);
                  else if Opponent_Display_Grid(I, J) >= 2 then
                     Put (Ship_Symbol);
                  end if; end if; end if;

                  Put (" ");
               end loop;            

               Put("     ");


               for J in Player_Grid'Range(2) loop

                  if Player_Grid(I, J) = 0 then
                     Put (Water_Symbol);
                  else if Player_Grid(I, J) = 1 then
                     Put (Miss_Symbol);
                  else if Player_Grid(I, J) >= 2 then
                     Put (Ship_Symbol);
                  end if; end if; end if;

                  Put (" ");

               end loop;

               New_Line;

            end loop;
            Put ("   0 1 2 3 4 5 6 7 8 9      0 1 2 3 4 5 6 7 8 9");
            New_Line;




            New_Line;
            Put(" Enter coordinates:                     0-9 0-9");
            New_Line;
            Put( "  > ");


            declare 

            Valid_Input : Boolean := False;
            X_Coordinate : Integer;
            Y_Coordinate : Integer;

            begin 

               while not Valid_Input loop

                  declare 

                     Input_Line : String := Ada.Text_IO.Get_Line; 

                  begin

                     if Input_Line = "q" then
                        exit;
                     end if;

                  --    if input is not three characters long, then it is invalid
                     if Input_Line'Length /= 3 then
                        New_Line;
                        Put(" Invalid input. Must be 3 chars");
                        New_Line;

                     -- if input 1 and 3 are not numbers, then it is invalid
                     elsif not (Input_Line(1) in '0' ..'9') or not (Input_Line(3) in '0' .. '9') then
                        New_Line;
                        Put(" Invalid input. Try '4 5'");
                        New_Line;

                     --  elsif co-ord has already been guessed (in oppontn_grid_hidden), then it is invalid
                     elsif Opponent_Display_Grid(Integer'Value(Input_Line(1 .. 1)) + 1, Integer'Value(Input_Line(3 .. 3)) + 1) /= 0 then
                        New_Line;
                        Put(" Invalid input.");
                        New_Line;
                        Put(" You have already guessed this coordinate");
                        New_Line;

                     else 
                        Valid_Input := True;
                        X_Coordinate := Integer'Value(Input_Line(1 .. 1)) + 1;
                        Y_Coordinate := Integer'Value(Input_Line(3 .. 3)) + 1;
                     end if;

                     if not Valid_Input then
                        New_Line;
                        Put(" Enter coordinates:                     0-9 0-9");
                        New_Line;
                        Put( "  > ");
                        end if;

                  end;

               end loop;

               New_Line;

               if Opponent_Grid(X_Coordinate, Y_Coordinate) = 0 then
                  Put("   Miss!");
                  Opponent_Display_Grid(X_Coordinate, Y_Coordinate) := 1;
               end if;
               if Opponent_Grid(X_Coordinate, Y_Coordinate) > 0 then
                  Put("   Hit! " & Integer'Image(Opponent_Grid(X_Coordinate, Y_Coordinate))); 
                  Opponent_Display_Grid(X_Coordinate, Y_Coordinate) := Opponent_Grid(X_Coordinate, Y_Coordinate);
                  --  Opponent_Display_Grid(X_Coordinate, Y_Coordinate) := Player_Ships(Opponent_Grid(X_Coordinate, Y_Coordinate).Ship_Element.Lookup_Index) + 2;
               end if;


               
               --  .Is_Ship then
               --    Put("   Hit! " & To_String(Opponent_Grid(X_Coordinate, Y_Coordinate).Ship_Element.Name)); 
               --    Opponent_Display_Grid(X_Coordinate, Y_Coordinate) := Player_Ships(Opponent_Grid(X_Coordinate, Y_Coordinate).Ship_Element.Lookup_Index) + 2;
               --  --    Opponent_Grid(X_Coordinate, Y_Coordinate).Ship_Element.Lookup_Index + 2;
               --  else
               --     Put("   Miss!");
               --     Opponent_Display_Grid(X_Coordinate, Y_Coordinate) := 1;
               --  end if;

               New_Line;
               New_Line;


            end;



         end loop;


      end;


end My_Hello_World;
