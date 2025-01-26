with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;

procedure My_Hello_World is
   --  type X_Range is (A, B, C, D, E, F, G, H, I, J);
   --  type Y_Range is range 0..10;
   --  type Index is range 1..5;

   --  type My_Int_Array is 
   --     array (Index) of Int_Range;

   --  Arr : My_Int_Array := (2, 3, 5, 7, 11);

   --  V : Int_Range;
   --  
   --  

   --  type Month_Duration is range 1 .. 31;
   --  type Month is (January, February, March, 
   --                 April, May, June, July, 
   --                 August, September, October, 
   --                 November, December);


   --  type My_Int_Array is 
   --     array (Month) of Month_Duration;

   --  Tab : constant My_Int_Array :=
   --     (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

   --  Feb_Days : Month_Duration := Tab (February);


   type My_Int is range 0 .. 1000;

   type X_Label is (A, B, C);

   type My_Int_Array is 
      array (0 .. 4) of My_Int;

   type List_Of_Lists is array (X_Label) of My_Int_Array;

   True_Board : List_Of_Lists :=
      ((0, 0, 1, 1, 1),
      (0, 0, 1, 1, 0),
      (1, 1, 1, 1, 0));

   Visible_Board : List_Of_Lists :=
      ((0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0));

   type User_Input is new String (1 .. 10);


begin

   --  for I in Tab'Range loop
   --     Put (My_Int'Image (Tab (I)));
   --  end loop;

   loop
   for I in Visible_Board'Range loop
      for J in Visible_Board(I)'Range loop
         Put (X_Label'Image (I));
         Put (Integer'Image (J));
         Put (" has value");
         Put (My_Int'Image (Visible_Board(I)(J)));
         New_Line;
      end loop;
   end loop;


   New_Line;

      declare
         Input_Line : String := Ada.Text_IO.Get_Line;
      begin -- Input_Loop
            exit when Input_Line = "exit";
            Ada.Text_IO.Put_Line ("You entered: " & Input_Line);

            --  split the first and second characters into individual variables
            declare
               First_Char : Character := Input_Line(1);
               Second_Char : Character := Input_Line(2);
            begin
               Ada.Text_IO.Put_Line ("First character: " & Character'Image (First_Char));
               Ada.Text_IO.Put_Line ("Second character: " & Character'Image (Second_Char));

               declare
                    X : X_Label := X_Label'Value (First_Char'Image(2 .. First_Char'Image'Length - 1));
                    Y : Integer := Integer'Value (Second_Char'Image(2 .. Second_Char'Image'Length - 1));
               begin
                  Ada.Text_IO.Put_Line ("X: " & X_Label'Image (X));
                  Ada.Text_IO.Put_Line ("Y: " & Integer'Image (Y));

                  Visible_Board(X)(Y) := True_Board(X)(Y);
               end;
            end;

      end;
   end loop;

   --  for M in Month loop
   --     Put_Line
   --        (Month'Image (M) & " has" & Month_Duration'Image (Tab (M)) & " days."); 
   --  end loop;
  
   Put_Line ("Finished!");
end My_Hello_World;