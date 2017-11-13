// ------------------------------------------//
// Neural Network Layer Generator - Part 1     
// ------------------------------------------//
// NAME:  Atif Iqbal                           
// NETID: aahangar                             
// SBUID: 111416569                            
// ------------------------------------------//


module layer_5_2_1_9 #(
   parameter M = 5,
   parameter N = 2,
   parameter P = 1,
   parameter T = 9)
(
   input  logic                   clk,
   input  logic                   reset,
   input  logic                   s_valid,
   input  logic                   m_ready,
   input  logic signed [T-1:0]    data_in,
   output logic                   m_valid,
   output logic                   s_ready,
   output logic signed [T-1:0]    data_out
);

   localparam MAT_W_SIZE = M * N;
   localparam VEC_b_SIZE = M;
   localparam VEC_x_SIZE = N;
   localparam VEC_y_SIZE = M;
   localparam MAT_W_ADDW = $clog2(MAT_W_SIZE);
   localparam VEC_b_ADDW = $clog2(VEC_b_SIZE);
   localparam VEC_x_ADDW = $clog2(VEC_x_SIZE+1);
   localparam VEC_y_ADDW = $clog2(VEC_y_SIZE);

   enum logic [1:0] {GET_x=0, COMPUTE_y=1} state, next_state;
   logic        [MAT_W_ADDW-1:0] rom_w_rd_addr;
   logic        [VEC_b_ADDW-1:0] rom_b_rd_addr;
   logic        [VEC_x_ADDW-1:0] ram_x_addr;
   logic        [VEC_x_ADDW-1:0] ram_x_wr_addr;
   logic        [VEC_x_ADDW-1:0] ram_x_rd_addr;
   logic signed          [T-1:0] rom_w_data_out;
   logic                 [T-1:0] rom_b_data_out;
   logic signed          [T-1:0] ram_x_data_out;
   logic signed          [T-1:0] mac_data_out;
   logic                         ram_x_wr_en;
   logic        [VEC_x_ADDW-1:0] vec_cnt;
   logic                         next_req;
   logic                         mac_valid_in;
   logic                         mac_valid_out;
   logic                         compute_done;

   assign s_ready = (state == GET_x);
   assign ram_x_wr_en = s_ready & s_valid;
   assign ram_x_addr = (state == GET_x) ? ram_x_wr_addr : ram_x_rd_addr;
   assign data_out = (mac_data_out < $signed(0)) ? 0 : mac_data_out;

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out        ),
      .addr     ( ram_x_addr            ),
      .wr_en    ( ram_x_wr_en           ));

   layer_5_2_1_9_W_rom u_w_rom (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr         ),
       .z       ( rom_w_data_out        ));

   layer_5_2_1_9_B_rom u_b_rom (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr         ),
       .z       ( rom_b_data_out        ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out       ),
      .b         ( rom_b_data_out       ),
      .x         ( ram_x_data_out       ),
      .valid_in  ( mac_valid_in         ),
      .f         ( mac_data_out         ),
      .valid_out ( mac_valid_out        ),
      .overflow  ( /* Not Used */       ));

   always_ff @(posedge clk)
      if(reset) begin
          state <= GET_x;
      end
      else begin
          state <= next_state;
      end

   always_comb begin
      next_state = GET_x;
      case (state)
          GET_x: begin
             if(ram_x_wr_addr == VEC_x_SIZE-1 && s_valid)
                next_state = COMPUTE_y;
             else
                next_state = GET_x;
          end
          COMPUTE_y: begin
             if(compute_done && m_valid && m_ready)
                next_state = GET_x;
             else
                next_state = COMPUTE_y;
          end
      endcase
   end

   always_ff @(posedge clk)
      if(reset) begin
         ram_x_wr_addr <= 'd0;
      end
      else begin
         if(ram_x_wr_addr == VEC_x_SIZE-1 && s_valid) begin
             ram_x_wr_addr <= 'd0;
         end
         else if (state == GET_x && s_valid) begin
             ram_x_wr_addr <= ram_x_wr_addr + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr <= 'd0;
         rom_b_rd_addr <= 'd0;
         ram_x_rd_addr <= 'd0;
         compute_done  <= 'd0;
      end
      else begin
         if(rom_w_rd_addr == MAT_W_SIZE-1 && next_req) begin
            rom_w_rd_addr <= 'd0;
            rom_b_rd_addr <= 'd0;
            ram_x_rd_addr <= 'd0;
            compute_done  <= 'd1;
         end
         else if (ram_x_rd_addr == VEC_x_SIZE-1 && next_req) begin
            rom_w_rd_addr <= rom_w_rd_addr + 1'd1;
            rom_b_rd_addr <= rom_b_rd_addr + 1'd1;
            ram_x_rd_addr <= 'd0;
            compute_done  <= 'd0;
         end
         else if ((state == COMPUTE_y) && vec_cnt < VEC_x_SIZE && next_req) begin
            rom_w_rd_addr <= rom_w_rd_addr + 1'd1;
            ram_x_rd_addr <= ram_x_rd_addr + 1'd1;
            compute_done  <= 'd0;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req     <= 1'b1;
         mac_valid_in <= 1'b0;
         vec_cnt      <=  'd0;
      end
      else begin
         if(vec_cnt == VEC_x_SIZE) begin
            next_req      <= 1'b0;
            mac_valid_in  <= 1'b0;
            vec_cnt       <= 2'd0;
         end
         else if (m_valid && m_ready) begin
            next_req      <= 1'b1;
         end
         else if (next_req && (state == COMPUTE_y)) begin
            next_req      <= 1'b1;
            mac_valid_in  <= 1'b1;
            vec_cnt       <= vec_cnt + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         m_valid <= 1'b0;
      end
      else begin
         if(mac_valid_out) begin
            m_valid <= 1'b1;
         end
         else if(m_valid && m_ready) begin
            m_valid <= 1'b0;
         end
      end

endmodule

module layer_5_2_1_9_W_rom(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [8:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 9'd7;
        1: z <= 9'd4;
        2: z <= -9'd6;
        3: z <= -9'd4;
        4: z <= -9'd1;
        5: z <= 9'd6;
        6: z <= 9'd6;
        7: z <= -9'd7;
        8: z <= -9'd6;
        9: z <= -9'd8;
      endcase
   end
endmodule

module layer_5_2_1_9_B_rom(clk, addr, z);
   input clk;
   input [2:0] addr;
   output logic signed [8:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 9'd2;
        1: z <= -9'd1;
        2: z <= 9'd6;
        3: z <= -9'd8;
        4: z <= 9'd1;
      endcase
   end
endmodule

//-----------------------------------------------------//
// MAC (Multiply & Accumulate Unit)
//-----------------------------------------------------//

module part3_mac #(parameter T = 8,
                   parameter NUM_S =  2,
                   parameter VEC_S =  3)
(
    input  logic                clk, 
    input  logic                reset,
    input  logic signed [T-1:0] a, 
    input  logic signed [T-1:0] b,  
    input  logic        [T-1:0] x,  
    input  logic                valid_in,
    output logic signed [T-1:0] f, 
    output logic                valid_out,
    output logic                overflow
);

    localparam VCNT_LSIZE  = $clog2(VEC_S+1);

    logic signed [T-1:0]    a_int;
    logic signed [T-1:0]    b_int;
    logic        [T-1:0]    x_int;
    logic signed [T-1:0]    c_int;
    logic signed [T-1:0]    d_int;
    logic signed [T-1:0]    e_int;
    logic                   overflow_int;
    logic                   enable_d;
    logic                   enable_f;
    logic [NUM_S-1:0]       enable_m;
    logic [VCNT_LSIZE-1:0]  vec_cnt;

    assign e_int = f + d_int;

    //Simple overflow detection logic
    assign overflow_int = ( f[T-1] &  d_int[T-1] & !e_int[T-1]) |
                          (!f[T-1] & !d_int[T-1] &  e_int[T-1]);

    generate
        if (NUM_S == 1) begin
            assign c_int = (a_int * x_int);
            assign enable_m[0] = enable_d;
        end
        else begin 
            if (NUM_S == 2) begin
                DW02_mult_2_stage #(8, 8) multinstance(a_int, x_int, 1'b1, clk, c_int);
            end
            else if(NUM_S == 3) begin
                DW02_mult_3_stage #(8, 8) multinstance(a_int, x_int, 1'b1, clk, c_int);
            end
            else if(NUM_S == 4) begin
                DW02_mult_4_stage #(8, 8) multinstance(a_int, x_int, 1'b1, clk, c_int);
            end
            else if(NUM_S == 5) begin
                DW02_mult_5_stage #(8, 8) multinstance(a_int, x_int, 1'b1, clk, c_int);
            end
            else if(NUM_S == 6) begin
                DW02_mult_6_stage #(8, 8) multinstance(a_int, x_int, 1'b1, clk, c_int);
            end

            always_ff @(posedge clk)
                if (reset) begin
                    enable_m <= 'd0;
                end
                else begin
                    enable_m[NUM_S-2] <= enable_d;
                    for(int i = 0; i < NUM_S-2; i++)
                        enable_m[i] <= enable_m[i+1];
                end
        end
    endgenerate

    //--------------------------------------------------//
    // Flopping the a, b and valid_in input.
    //--------------------------------------------------//
    always_ff @(posedge clk)
        if (reset) begin
            a_int    <=  'd0;
            b_int    <=  'd0;
            x_int    <=  'd0;
            enable_d <= 1'b0;
        end
        else if (valid_in) begin
            a_int    <= a;
            b_int    <= b;
            x_int    <= x;
            enable_d <= 1'b1;
        end
        else begin
            enable_d <= 1'b0;
        end


    //--------------------------------------------------//
    // Pipeline reg between multiplier and adder.
    //--------------------------------------------------//
    always_ff @(posedge clk)
        if (reset) begin
            d_int    <=  'd0;
            enable_f <= 1'b0;
        end
        else if (enable_m[0]) begin
            d_int    <= c_int; 
            enable_f <= 1'b1;
        end
        else begin
            enable_f <= 1'b0;
        end

    //--------------------------------------------------//
    // Doing MAC operation.
    //--------------------------------------------------//
    always_ff @(posedge clk)
        if (reset) begin
            f         <=   'd0;
            valid_out <=  1'b0; 
            vec_cnt   <=   'd0;
        end
        else if (enable_f) begin
            f         <= (vec_cnt ==    2'd0) ? d_int + b_int : f + d_int;
            vec_cnt   <= (vec_cnt == VEC_S-1) ? 0 : vec_cnt + 1'b1;
            valid_out <= (vec_cnt == VEC_S-1) ? 1'b1 : 1'b0;
        end
        else begin
            valid_out <= 1'b0;
        end

    //--------------------------------------------------//
    // Overflow detection.
    //--------------------------------------------------//
    always_ff @(posedge clk)
        if (reset)
            overflow <= 1'b0; 
        else if (vec_cnt == 0)
            overflow <= 1'b0;
        else if (overflow_int & enable_f)
            overflow <= 1'b1;

endmodule
//end of file.
module memory(clk, data_in, data_out, addr, wr_en);
   
   parameter WIDTH=16, SIZE=64, LOGSIZE=6;
   input [WIDTH-1:0] data_in;
   output logic [WIDTH-1:0] data_out;
   input [LOGSIZE-1:0]      addr;
   input                    clk, wr_en;
   
   logic [SIZE-1:0][WIDTH-1:0] mem;
   
   always_ff @(posedge clk) begin
      data_out <= mem[addr];
	  if (wr_en)
	    mem[addr] <= data_in;
   end
endmodule