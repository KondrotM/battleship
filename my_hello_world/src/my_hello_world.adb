
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random; 

procedure My_Hello_World is

   type Coordinate is record
      X : Natural;
      Y : Natural;
   end record;

   type Grid is array (1 .. 10, 1 .. 10) of Natural;
   type Tile_Array is array (Positive range <>) of Coordinate;

   type Ship is tagged record
      Length : Natural;
      Hits : Natural := 0;
      Name: String(1 .. 3);
      Tiles: Tile_Array(1 .. 6);

   end record;

   procedure Place_Ship (
      S : in out Ship; 
      G : in out Grid; 
      C: Coordinate;
      Direction : String) is
   begin
      if Direction = "horizontal" then

         for I in 1 .. S.Length loop
            G(C.X, C.Y + I) := 1;
         end loop;

      elsif Direction = "vertical" then

         for I in 1 .. S.Length loop
            G(C.X + I, C.Y) := 1;
         end loop;

      else
         Ada.Text_IO.Put_Line("Invalid direction");

      end if;

   end Place_Ship;

   begin

      declare 
         --  Player1_Grid : Grid := (others => (others => 0));
         --  Player2_Grid : Grid := (others => (others => 0));
         
         Opponent_Grid: Grid := (others => (others => 0));
         Opponent_Grid_Hidden : Grid := (others => (others => 0));
         Opponent_Memory : Grid := (others => (others => 0));

         Player_Grid: Grid := (others => (others => 0));
         Player_Grid_Hidden : Grid := (others => (others => 0));

         Carrier : Ship;
         Battleship : Ship;
         Cruiser : Ship;
         Submarine : Ship;
         Destroyer : Ship;

         Water_Symbol : String := "~";
         Ship_Symbol : String := "X";
         Miss_Symbol : String := "O";

         X_Offset : Integer;

      begin


         
         Carrier.Length := 5;
         Carrier.Name := "Car";

         Battleship.Length := 4;
         Battleship.Name := "Bat";

         Cruiser.Length := 3;
         Cruiser.Name := "Cru";

         Submarine.Length := 3;
         Submarine.Name := "Sub";

         Destroyer.Length := 2;
         Destroyer.Name := "Des";

         Place_Ship(Carrier, Player_Grid, Coordinate'(1, 1), "horizontal");
         Place_Ship(Battleship, Player_Grid, Coordinate'(2, 1), "horizontal");
         Place_Ship(Cruiser, Player_Grid, Coordinate'(3, 1), "horizontal");
         Place_Ship(Submarine, Player_Grid, Coordinate'(4, 1), "horizontal");
         Place_Ship(Destroyer, Player_Grid, Coordinate'(5, 1), "horizontal");

         Place_Ship(Carrier, Opponent_Grid, Coordinate'(1, 1), "vertical");
         Place_Ship(Battleship, Opponent_Grid, Coordinate'(2, 1), "vertical");
         Place_Ship(Cruiser, Opponent_Grid, Coordinate'(3, 1), "vertical");
         Place_Ship(Submarine, Opponent_Grid, Coordinate'(4, 1), "vertical");
         Place_Ship(Destroyer, Opponent_Grid, Coordinate'(5, 1), "vertical");

         Put (" Opponent Grid              Player Grid");
         New_Line;
         New_Line;
         
         Put ("   0 1 2 3 4 5 6 7 8 9      0 1 2 3 4 5 6 7 8 9");
         New_Line;

         while True loop

            for I in Player_Grid'Range loop
               --  Put (Player_Grid(I, 2)'Image);

                  X_Offset := I - 1;

                  Put (X_Offset'Image);
                  Put (" ");

               for J in Opponent_Grid_Hidden'Range loop
                  if Opponent_Grid_Hidden(I, J) = 0 then
                     Put (Water_Symbol);
                  else if Opponent_Grid_Hidden(I, J) = 1 then
                     Put (Ship_Symbol);
                  else if Opponent_Grid_Hidden(I, J) = 2 then
                     Put (Miss_Symbol);
                  end if; end if; end if;

                  Put (" ");
               end loop;            

               Put("     ");


               for J in Player_Grid'Range(2) loop

                  if Player_Grid(I, J) = 0 then
                     Put (Water_Symbol);
                  else if Player_Grid(I, J) = 1 then
                     Put (Ship_Symbol);
                  else if Player_Grid(I, J) = 2 then
                     Put (Miss_Symbol);
                  end if; end if; end if;

                  Put (" ");

               end loop;

               New_Line;

            end loop;


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
                     elsif Opponent_Grid_Hidden(Integer'Value(Input_Line(1 .. 1)) + 1, Integer'Value(Input_Line(3 .. 3)) + 1) /= 0 then
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

               if Opponent_Grid(X_Coordinate, Y_Coordinate) = 1 then
                  Put("   Hit!");
                  Opponent_Grid_Hidden(X_Coordinate, Y_Coordinate) := 1;
               else
                  Put("   Miss!");
                  Opponent_Grid_Hidden(X_Coordinate, Y_Coordinate) := 2;
               end if;

               New_Line;
               New_Line;


            end;



         end loop;


      end;


end My_Hello_World;