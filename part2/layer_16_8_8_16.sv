// ------------------------------------------//
// Neural Network Layer Generator - Part 1     
// ------------------------------------------//
// NAME:  Atif Iqbal                           
// NETID: aahangar                             
// SBUID: 111416569                            
// ------------------------------------------//


module layer_16_8_8_16 #(
   parameter M = 16,
   parameter N = 8,
   parameter P = 8,
   parameter T = 16)
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

   localparam MAT_W_SIZE = (M * N) / P;
   localparam VEC_b_SIZE = M / P;
   localparam VEC_x_SIZE = N;
   localparam VEC_y_SIZE = M;
   localparam MAT_W_ADDW = $clog2(MAT_W_SIZE);
   localparam VEC_b_ADDW = $clog2(VEC_b_SIZE);
   localparam VEC_x_ADDW = $clog2(VEC_x_SIZE+1);
   localparam VEC_y_ADDW = $clog2(VEC_y_SIZE+1);

   enum logic [1:0] {GET_x=0, COMPUTE_y=1} state, next_state;
   logic        [MAT_W_ADDW-1:0] rom_w_rd_addr[P];
   logic        [VEC_b_ADDW-1:0] rom_b_rd_addr[P];
   logic        [VEC_x_ADDW-1:0] ram_x_addr[P];
   logic        [VEC_x_ADDW-1:0] ram_x_wr_addr;
   logic        [VEC_x_ADDW-1:0] ram_x_rd_addr[P];
   logic signed          [T-1:0] rom_w_data_out[P];
   logic                 [T-1:0] rom_b_data_out[P];
   logic signed          [T-1:0] ram_x_data_out[P];
   logic signed          [T-1:0] mac_data_out[P];
   logic                         ram_x_wr_en;
   logic        [VEC_x_ADDW-1:0] vec_cnt[P];
   logic                         next_req[P];
   logic                         mac_valid_in[P];
   logic                         mac_valid_out[P];
   logic                         valid_int[P];
   logic        [VEC_y_ADDW-1:0] output_cnt;
   logic        [VEC_y_ADDW-1:0] vld_out_cnt;
   logic        [VEC_y_ADDW-1:0] vld_in_cnt[P];
   logic                         compute_done;

   assign s_ready = (state == GET_x);
   assign ram_x_wr_en = s_ready & s_valid;

   always_comb begin
       data_out   = 'bx;
       m_valid    = 'bx;
       for(int i = 0; i < P; i++) begin
           ram_x_addr[i] = (state == GET_x) ? ram_x_wr_addr : ram_x_rd_addr[i];
           if(vld_out_cnt == i) begin
               data_out = (mac_data_out[i] < $signed(0)) ? 0 : mac_data_out[i];
               m_valid  = valid_int[i];
           end
       end
   end

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_0 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[0]     ),
      .addr     ( ram_x_addr[0]         ),
      .wr_en    ( ram_x_wr_en           ));

   layer_16_8_8_16_W_rom_0 u_w_rom_0 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[0]      ),
       .z       ( rom_w_data_out[0]     ));

   layer_16_8_8_16_B_rom_0 u_b_rom_0 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[0]      ),
       .z       ( rom_b_data_out[0]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_0 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[0]    ),
      .b         ( rom_b_data_out[0]    ),
      .x         ( ram_x_data_out[0]    ),
      .valid_in  ( mac_valid_in[0]      ),
      .f         ( mac_data_out[0]      ),
      .valid_out ( mac_valid_out[0]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_1 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[1]     ),
      .addr     ( ram_x_addr[1]         ),
      .wr_en    ( ram_x_wr_en           ));

   layer_16_8_8_16_W_rom_1 u_w_rom_1 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[1]      ),
       .z       ( rom_w_data_out[1]     ));

   layer_16_8_8_16_B_rom_1 u_b_rom_1 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[1]      ),
       .z       ( rom_b_data_out[1]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_1 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[1]    ),
      .b         ( rom_b_data_out[1]    ),
      .x         ( ram_x_data_out[1]    ),
      .valid_in  ( mac_valid_in[1]      ),
      .f         ( mac_data_out[1]      ),
      .valid_out ( mac_valid_out[1]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_2 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[2]     ),
      .addr     ( ram_x_addr[2]         ),
      .wr_en    ( ram_x_wr_en           ));

   layer_16_8_8_16_W_rom_2 u_w_rom_2 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[2]      ),
       .z       ( rom_w_data_out[2]     ));

   layer_16_8_8_16_B_rom_2 u_b_rom_2 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[2]      ),
       .z       ( rom_b_data_out[2]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_2 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[2]    ),
      .b         ( rom_b_data_out[2]    ),
      .x         ( ram_x_data_out[2]    ),
      .valid_in  ( mac_valid_in[2]      ),
      .f         ( mac_data_out[2]      ),
      .valid_out ( mac_valid_out[2]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_3 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[3]     ),
      .addr     ( ram_x_addr[3]         ),
      .wr_en    ( ram_x_wr_en           ));

   layer_16_8_8_16_W_rom_3 u_w_rom_3 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[3]      ),
       .z       ( rom_w_data_out[3]     ));

   layer_16_8_8_16_B_rom_3 u_b_rom_3 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[3]      ),
       .z       ( rom_b_data_out[3]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_3 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[3]    ),
      .b         ( rom_b_data_out[3]    ),
      .x         ( ram_x_data_out[3]    ),
      .valid_in  ( mac_valid_in[3]      ),
      .f         ( mac_data_out[3]      ),
      .valid_out ( mac_valid_out[3]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_4 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[4]     ),
      .addr     ( ram_x_addr[4]         ),
      .wr_en    ( ram_x_wr_en           ));

   layer_16_8_8_16_W_rom_4 u_w_rom_4 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[4]      ),
       .z       ( rom_w_data_out[4]     ));

   layer_16_8_8_16_B_rom_4 u_b_rom_4 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[4]      ),
       .z       ( rom_b_data_out[4]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_4 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[4]    ),
      .b         ( rom_b_data_out[4]    ),
      .x         ( ram_x_data_out[4]    ),
      .valid_in  ( mac_valid_in[4]      ),
      .f         ( mac_data_out[4]      ),
      .valid_out ( mac_valid_out[4]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_5 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[5]     ),
      .addr     ( ram_x_addr[5]         ),
      .wr_en    ( ram_x_wr_en           ));

   layer_16_8_8_16_W_rom_5 u_w_rom_5 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[5]      ),
       .z       ( rom_w_data_out[5]     ));

   layer_16_8_8_16_B_rom_5 u_b_rom_5 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[5]      ),
       .z       ( rom_b_data_out[5]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_5 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[5]    ),
      .b         ( rom_b_data_out[5]    ),
      .x         ( ram_x_data_out[5]    ),
      .valid_in  ( mac_valid_in[5]      ),
      .f         ( mac_data_out[5]      ),
      .valid_out ( mac_valid_out[5]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_6 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[6]     ),
      .addr     ( ram_x_addr[6]         ),
      .wr_en    ( ram_x_wr_en           ));

   layer_16_8_8_16_W_rom_6 u_w_rom_6 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[6]      ),
       .z       ( rom_w_data_out[6]     ));

   layer_16_8_8_16_B_rom_6 u_b_rom_6 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[6]      ),
       .z       ( rom_b_data_out[6]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_6 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[6]    ),
      .b         ( rom_b_data_out[6]    ),
      .x         ( ram_x_data_out[6]    ),
      .valid_in  ( mac_valid_in[6]      ),
      .f         ( mac_data_out[6]      ),
      .valid_out ( mac_valid_out[6]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_7 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[7]     ),
      .addr     ( ram_x_addr[7]         ),
      .wr_en    ( ram_x_wr_en           ));

   layer_16_8_8_16_W_rom_7 u_w_rom_7 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[7]      ),
       .z       ( rom_w_data_out[7]     ));

   layer_16_8_8_16_B_rom_7 u_b_rom_7 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[7]      ),
       .z       ( rom_b_data_out[7]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_7 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[7]    ),
      .b         ( rom_b_data_out[7]    ),
      .x         ( ram_x_data_out[7]    ),
      .valid_in  ( mac_valid_in[7]      ),
      .f         ( mac_data_out[7]      ),
      .valid_out ( mac_valid_out[7]     ),
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
             if(compute_done)
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
         rom_w_rd_addr[0] <= 'd0;
         rom_b_rd_addr[0] <= 'd0;
         ram_x_rd_addr[0] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[0] == MAT_W_SIZE-1 && next_req[0]) begin
            ram_x_rd_addr[0] <= 'd0;
            rom_w_rd_addr[0] <= 'd0;
            rom_b_rd_addr[0] <= 'd0;
         end
         else if (ram_x_rd_addr[ 0] == VEC_x_SIZE-1 && next_req[0]) begin
            ram_x_rd_addr[0] <= 'd0;
            rom_w_rd_addr[0] <= rom_w_rd_addr[0] + 1'b1;
            rom_b_rd_addr[0] <= rom_b_rd_addr[0] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[0] < VEC_x_SIZE && next_req[0]) begin
            ram_x_rd_addr[0] <= ram_x_rd_addr[0] + 1'b1;
            rom_w_rd_addr[0] <= rom_w_rd_addr[0] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[1] <= 'd0;
         rom_b_rd_addr[1] <= 'd0;
         ram_x_rd_addr[1] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[1] == MAT_W_SIZE-1 && next_req[1]) begin
            ram_x_rd_addr[1] <= 'd0;
            rom_w_rd_addr[1] <= 'd0;
            rom_b_rd_addr[1] <= 'd0;
         end
         else if (ram_x_rd_addr[ 1] == VEC_x_SIZE-1 && next_req[1]) begin
            ram_x_rd_addr[1] <= 'd0;
            rom_w_rd_addr[1] <= rom_w_rd_addr[1] + 1'b1;
            rom_b_rd_addr[1] <= rom_b_rd_addr[1] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[1] < VEC_x_SIZE && next_req[1]) begin
            ram_x_rd_addr[1] <= ram_x_rd_addr[1] + 1'b1;
            rom_w_rd_addr[1] <= rom_w_rd_addr[1] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[2] <= 'd0;
         rom_b_rd_addr[2] <= 'd0;
         ram_x_rd_addr[2] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[2] == MAT_W_SIZE-1 && next_req[2]) begin
            ram_x_rd_addr[2] <= 'd0;
            rom_w_rd_addr[2] <= 'd0;
            rom_b_rd_addr[2] <= 'd0;
         end
         else if (ram_x_rd_addr[ 2] == VEC_x_SIZE-1 && next_req[2]) begin
            ram_x_rd_addr[2] <= 'd0;
            rom_w_rd_addr[2] <= rom_w_rd_addr[2] + 1'b1;
            rom_b_rd_addr[2] <= rom_b_rd_addr[2] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[2] < VEC_x_SIZE && next_req[2]) begin
            ram_x_rd_addr[2] <= ram_x_rd_addr[2] + 1'b1;
            rom_w_rd_addr[2] <= rom_w_rd_addr[2] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[3] <= 'd0;
         rom_b_rd_addr[3] <= 'd0;
         ram_x_rd_addr[3] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[3] == MAT_W_SIZE-1 && next_req[3]) begin
            ram_x_rd_addr[3] <= 'd0;
            rom_w_rd_addr[3] <= 'd0;
            rom_b_rd_addr[3] <= 'd0;
         end
         else if (ram_x_rd_addr[ 3] == VEC_x_SIZE-1 && next_req[3]) begin
            ram_x_rd_addr[3] <= 'd0;
            rom_w_rd_addr[3] <= rom_w_rd_addr[3] + 1'b1;
            rom_b_rd_addr[3] <= rom_b_rd_addr[3] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[3] < VEC_x_SIZE && next_req[3]) begin
            ram_x_rd_addr[3] <= ram_x_rd_addr[3] + 1'b1;
            rom_w_rd_addr[3] <= rom_w_rd_addr[3] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[4] <= 'd0;
         rom_b_rd_addr[4] <= 'd0;
         ram_x_rd_addr[4] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[4] == MAT_W_SIZE-1 && next_req[4]) begin
            ram_x_rd_addr[4] <= 'd0;
            rom_w_rd_addr[4] <= 'd0;
            rom_b_rd_addr[4] <= 'd0;
         end
         else if (ram_x_rd_addr[ 4] == VEC_x_SIZE-1 && next_req[4]) begin
            ram_x_rd_addr[4] <= 'd0;
            rom_w_rd_addr[4] <= rom_w_rd_addr[4] + 1'b1;
            rom_b_rd_addr[4] <= rom_b_rd_addr[4] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[4] < VEC_x_SIZE && next_req[4]) begin
            ram_x_rd_addr[4] <= ram_x_rd_addr[4] + 1'b1;
            rom_w_rd_addr[4] <= rom_w_rd_addr[4] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[5] <= 'd0;
         rom_b_rd_addr[5] <= 'd0;
         ram_x_rd_addr[5] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[5] == MAT_W_SIZE-1 && next_req[5]) begin
            ram_x_rd_addr[5] <= 'd0;
            rom_w_rd_addr[5] <= 'd0;
            rom_b_rd_addr[5] <= 'd0;
         end
         else if (ram_x_rd_addr[ 5] == VEC_x_SIZE-1 && next_req[5]) begin
            ram_x_rd_addr[5] <= 'd0;
            rom_w_rd_addr[5] <= rom_w_rd_addr[5] + 1'b1;
            rom_b_rd_addr[5] <= rom_b_rd_addr[5] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[5] < VEC_x_SIZE && next_req[5]) begin
            ram_x_rd_addr[5] <= ram_x_rd_addr[5] + 1'b1;
            rom_w_rd_addr[5] <= rom_w_rd_addr[5] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[6] <= 'd0;
         rom_b_rd_addr[6] <= 'd0;
         ram_x_rd_addr[6] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[6] == MAT_W_SIZE-1 && next_req[6]) begin
            ram_x_rd_addr[6] <= 'd0;
            rom_w_rd_addr[6] <= 'd0;
            rom_b_rd_addr[6] <= 'd0;
         end
         else if (ram_x_rd_addr[ 6] == VEC_x_SIZE-1 && next_req[6]) begin
            ram_x_rd_addr[6] <= 'd0;
            rom_w_rd_addr[6] <= rom_w_rd_addr[6] + 1'b1;
            rom_b_rd_addr[6] <= rom_b_rd_addr[6] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[6] < VEC_x_SIZE && next_req[6]) begin
            ram_x_rd_addr[6] <= ram_x_rd_addr[6] + 1'b1;
            rom_w_rd_addr[6] <= rom_w_rd_addr[6] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[7] <= 'd0;
         rom_b_rd_addr[7] <= 'd0;
         ram_x_rd_addr[7] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[7] == MAT_W_SIZE-1 && next_req[7]) begin
            ram_x_rd_addr[7] <= 'd0;
            rom_w_rd_addr[7] <= 'd0;
            rom_b_rd_addr[7] <= 'd0;
         end
         else if (ram_x_rd_addr[ 7] == VEC_x_SIZE-1 && next_req[7]) begin
            ram_x_rd_addr[7] <= 'd0;
            rom_w_rd_addr[7] <= rom_w_rd_addr[7] + 1'b1;
            rom_b_rd_addr[7] <= rom_b_rd_addr[7] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[7] < VEC_x_SIZE && next_req[7]) begin
            ram_x_rd_addr[7] <= ram_x_rd_addr[7] + 1'b1;
            rom_w_rd_addr[7] <= rom_w_rd_addr[7] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         output_cnt    <= 'd0;
         compute_done  <= 'd0;
      end
      else begin
         if (state == GET_x) begin
            output_cnt   <= 'd0;
            compute_done  <= 'd0;
         end
         else if(output_cnt == M) begin
            compute_done <= 'd1;
         end
         else if ((state == COMPUTE_y) && m_valid && m_ready) begin
            output_cnt   <= output_cnt + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[0]     <= 1'b1;
         mac_valid_in[0] <= 1'b0;
         vec_cnt[0]      <=  'd0;
         vld_in_cnt[0]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[0]   <=  'd0;
            next_req[0]     <= 1'b1;
         end
         else if(vld_in_cnt[0] == VEC_b_SIZE) begin
            next_req[0]      <= 1'b0;
            mac_valid_in[0]  <= 1'b0;
            vec_cnt[0]       <= 2'd0;
         end
         else if(vec_cnt[0] == VEC_x_SIZE) begin
            next_req[0]      <= 1'b0;
            mac_valid_in[0]  <= 1'b0;
            vec_cnt[0]       <= 2'd0;
            vld_in_cnt[0]    <= vld_in_cnt[0] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 0) && (vld_in_cnt[0] < VEC_b_SIZE)) begin
            next_req[0]      <= 1'b1;
         end
         else if (next_req[0] && (state == COMPUTE_y)) begin
            next_req[0]      <= 1'b1;
            mac_valid_in[0]  <= 1'b1;
            vec_cnt[0]       <= vec_cnt[0] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[1]     <= 1'b1;
         mac_valid_in[1] <= 1'b0;
         vec_cnt[1]      <=  'd0;
         vld_in_cnt[1]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[1]   <=  'd0;
            next_req[1]     <= 1'b1;
         end
         else if(vld_in_cnt[1] == VEC_b_SIZE) begin
            next_req[1]      <= 1'b0;
            mac_valid_in[1]  <= 1'b0;
            vec_cnt[1]       <= 2'd0;
         end
         else if(vec_cnt[1] == VEC_x_SIZE) begin
            next_req[1]      <= 1'b0;
            mac_valid_in[1]  <= 1'b0;
            vec_cnt[1]       <= 2'd0;
            vld_in_cnt[1]    <= vld_in_cnt[1] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 1) && (vld_in_cnt[1] < VEC_b_SIZE)) begin
            next_req[1]      <= 1'b1;
         end
         else if (next_req[1] && (state == COMPUTE_y)) begin
            next_req[1]      <= 1'b1;
            mac_valid_in[1]  <= 1'b1;
            vec_cnt[1]       <= vec_cnt[1] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[2]     <= 1'b1;
         mac_valid_in[2] <= 1'b0;
         vec_cnt[2]      <=  'd0;
         vld_in_cnt[2]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[2]   <=  'd0;
            next_req[2]     <= 1'b1;
         end
         else if(vld_in_cnt[2] == VEC_b_SIZE) begin
            next_req[2]      <= 1'b0;
            mac_valid_in[2]  <= 1'b0;
            vec_cnt[2]       <= 2'd0;
         end
         else if(vec_cnt[2] == VEC_x_SIZE) begin
            next_req[2]      <= 1'b0;
            mac_valid_in[2]  <= 1'b0;
            vec_cnt[2]       <= 2'd0;
            vld_in_cnt[2]    <= vld_in_cnt[2] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 2) && (vld_in_cnt[2] < VEC_b_SIZE)) begin
            next_req[2]      <= 1'b1;
         end
         else if (next_req[2] && (state == COMPUTE_y)) begin
            next_req[2]      <= 1'b1;
            mac_valid_in[2]  <= 1'b1;
            vec_cnt[2]       <= vec_cnt[2] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[3]     <= 1'b1;
         mac_valid_in[3] <= 1'b0;
         vec_cnt[3]      <=  'd0;
         vld_in_cnt[3]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[3]   <=  'd0;
            next_req[3]     <= 1'b1;
         end
         else if(vld_in_cnt[3] == VEC_b_SIZE) begin
            next_req[3]      <= 1'b0;
            mac_valid_in[3]  <= 1'b0;
            vec_cnt[3]       <= 2'd0;
         end
         else if(vec_cnt[3] == VEC_x_SIZE) begin
            next_req[3]      <= 1'b0;
            mac_valid_in[3]  <= 1'b0;
            vec_cnt[3]       <= 2'd0;
            vld_in_cnt[3]    <= vld_in_cnt[3] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 3) && (vld_in_cnt[3] < VEC_b_SIZE)) begin
            next_req[3]      <= 1'b1;
         end
         else if (next_req[3] && (state == COMPUTE_y)) begin
            next_req[3]      <= 1'b1;
            mac_valid_in[3]  <= 1'b1;
            vec_cnt[3]       <= vec_cnt[3] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[4]     <= 1'b1;
         mac_valid_in[4] <= 1'b0;
         vec_cnt[4]      <=  'd0;
         vld_in_cnt[4]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[4]   <=  'd0;
            next_req[4]     <= 1'b1;
         end
         else if(vld_in_cnt[4] == VEC_b_SIZE) begin
            next_req[4]      <= 1'b0;
            mac_valid_in[4]  <= 1'b0;
            vec_cnt[4]       <= 2'd0;
         end
         else if(vec_cnt[4] == VEC_x_SIZE) begin
            next_req[4]      <= 1'b0;
            mac_valid_in[4]  <= 1'b0;
            vec_cnt[4]       <= 2'd0;
            vld_in_cnt[4]    <= vld_in_cnt[4] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 4) && (vld_in_cnt[4] < VEC_b_SIZE)) begin
            next_req[4]      <= 1'b1;
         end
         else if (next_req[4] && (state == COMPUTE_y)) begin
            next_req[4]      <= 1'b1;
            mac_valid_in[4]  <= 1'b1;
            vec_cnt[4]       <= vec_cnt[4] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[5]     <= 1'b1;
         mac_valid_in[5] <= 1'b0;
         vec_cnt[5]      <=  'd0;
         vld_in_cnt[5]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[5]   <=  'd0;
            next_req[5]     <= 1'b1;
         end
         else if(vld_in_cnt[5] == VEC_b_SIZE) begin
            next_req[5]      <= 1'b0;
            mac_valid_in[5]  <= 1'b0;
            vec_cnt[5]       <= 2'd0;
         end
         else if(vec_cnt[5] == VEC_x_SIZE) begin
            next_req[5]      <= 1'b0;
            mac_valid_in[5]  <= 1'b0;
            vec_cnt[5]       <= 2'd0;
            vld_in_cnt[5]    <= vld_in_cnt[5] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 5) && (vld_in_cnt[5] < VEC_b_SIZE)) begin
            next_req[5]      <= 1'b1;
         end
         else if (next_req[5] && (state == COMPUTE_y)) begin
            next_req[5]      <= 1'b1;
            mac_valid_in[5]  <= 1'b1;
            vec_cnt[5]       <= vec_cnt[5] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[6]     <= 1'b1;
         mac_valid_in[6] <= 1'b0;
         vec_cnt[6]      <=  'd0;
         vld_in_cnt[6]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[6]   <=  'd0;
            next_req[6]     <= 1'b1;
         end
         else if(vld_in_cnt[6] == VEC_b_SIZE) begin
            next_req[6]      <= 1'b0;
            mac_valid_in[6]  <= 1'b0;
            vec_cnt[6]       <= 2'd0;
         end
         else if(vec_cnt[6] == VEC_x_SIZE) begin
            next_req[6]      <= 1'b0;
            mac_valid_in[6]  <= 1'b0;
            vec_cnt[6]       <= 2'd0;
            vld_in_cnt[6]    <= vld_in_cnt[6] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 6) && (vld_in_cnt[6] < VEC_b_SIZE)) begin
            next_req[6]      <= 1'b1;
         end
         else if (next_req[6] && (state == COMPUTE_y)) begin
            next_req[6]      <= 1'b1;
            mac_valid_in[6]  <= 1'b1;
            vec_cnt[6]       <= vec_cnt[6] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[7]     <= 1'b1;
         mac_valid_in[7] <= 1'b0;
         vec_cnt[7]      <=  'd0;
         vld_in_cnt[7]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[7]   <=  'd0;
            next_req[7]     <= 1'b1;
         end
         else if(vld_in_cnt[7] == VEC_b_SIZE) begin
            next_req[7]      <= 1'b0;
            mac_valid_in[7]  <= 1'b0;
            vec_cnt[7]       <= 2'd0;
         end
         else if(vec_cnt[7] == VEC_x_SIZE) begin
            next_req[7]      <= 1'b0;
            mac_valid_in[7]  <= 1'b0;
            vec_cnt[7]       <= 2'd0;
            vld_in_cnt[7]    <= vld_in_cnt[7] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 7) && (vld_in_cnt[7] < VEC_b_SIZE)) begin
            next_req[7]      <= 1'b1;
         end
         else if (next_req[7] && (state == COMPUTE_y)) begin
            next_req[7]      <= 1'b1;
            mac_valid_in[7]  <= 1'b1;
            vec_cnt[7]       <= vec_cnt[7] + 1'b1;
         end
      end

   always_ff @(posedge clk)
       if(reset) begin
           vld_out_cnt <= 'd0;
       end
       else begin
           if(vld_out_cnt == P-1 && m_valid && m_ready) begin
               vld_out_cnt <= 'd0;
           end
           else if(m_valid & m_ready) begin
               vld_out_cnt <= vld_out_cnt + 1'b1;
           end
       end

   always_ff @(posedge clk)
      if(reset) begin
         for(int i = 0; i < P; i++) begin
            valid_int[i] <= 1'b0;
         end
      end
      else begin
         if(m_valid && m_ready) begin
            for(int i = 0; i < P; i++) begin
               if(vld_out_cnt == i) valid_int[i] <= 1'b0;
               else                 valid_int[i] <= (mac_valid_out[i]) ? 1'b1 : valid_int[i];
            end
         end
         else begin
            for(int i = 0; i < P; i++) begin
               valid_int[i] <= (mac_valid_out[i]) ? 1'b1 : valid_int[i];
            end
         end
      end

endmodule

module layer_16_8_8_16_W_rom_0(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd91;
        1: z <= -16'd103;
        2: z <= 16'd89;
        3: z <= 16'd48;
        4: z <= 16'd35;
        5: z <= 16'd12;
        6: z <= 16'd112;
        7: z <= -16'd16;
        8: z <= -16'd87;
        9: z <= -16'd11;
        10: z <= 16'd121;
        11: z <= 16'd102;
        12: z <= 16'd30;
        13: z <= -16'd60;
        14: z <= 16'd52;
        15: z <= 16'd101;
      endcase
   end
endmodule

module layer_16_8_8_16_B_rom_0(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd34;
        1: z <= 16'd22;
      endcase
   end
endmodule

module layer_16_8_8_16_W_rom_1(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd111;
        1: z <= -16'd99;
        2: z <= -16'd108;
        3: z <= -16'd54;
        4: z <= -16'd90;
        5: z <= 16'd12;
        6: z <= -16'd95;
        7: z <= 16'd77;
        8: z <= 16'd35;
        9: z <= 16'd77;
        10: z <= -16'd46;
        11: z <= 16'd14;
        12: z <= -16'd122;
        13: z <= 16'd12;
        14: z <= -16'd17;
        15: z <= -16'd60;
      endcase
   end
endmodule

module layer_16_8_8_16_B_rom_1(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd31;
        1: z <= -16'd68;
      endcase
   end
endmodule

module layer_16_8_8_16_W_rom_2(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd117;
        1: z <= -16'd124;
        2: z <= -16'd98;
        3: z <= -16'd72;
        4: z <= -16'd74;
        5: z <= 16'd18;
        6: z <= -16'd8;
        7: z <= -16'd94;
        8: z <= 16'd101;
        9: z <= 16'd8;
        10: z <= 16'd56;
        11: z <= 16'd79;
        12: z <= -16'd102;
        13: z <= -16'd50;
        14: z <= 16'd30;
        15: z <= -16'd24;
      endcase
   end
endmodule

module layer_16_8_8_16_B_rom_2(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd39;
        1: z <= -16'd78;
      endcase
   end
endmodule

module layer_16_8_8_16_W_rom_3(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd100;
        1: z <= 16'd60;
        2: z <= -16'd78;
        3: z <= 16'd83;
        4: z <= -16'd28;
        5: z <= -16'd98;
        6: z <= -16'd41;
        7: z <= 16'd9;
        8: z <= -16'd105;
        9: z <= -16'd65;
        10: z <= -16'd102;
        11: z <= 16'd125;
        12: z <= -16'd72;
        13: z <= -16'd43;
        14: z <= -16'd101;
        15: z <= -16'd31;
      endcase
   end
endmodule

module layer_16_8_8_16_B_rom_3(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd54;
        1: z <= 16'd68;
      endcase
   end
endmodule

module layer_16_8_8_16_W_rom_4(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd73;
        1: z <= -16'd80;
        2: z <= -16'd71;
        3: z <= 16'd91;
        4: z <= 16'd61;
        5: z <= -16'd87;
        6: z <= -16'd53;
        7: z <= 16'd78;
        8: z <= 16'd74;
        9: z <= -16'd108;
        10: z <= -16'd57;
        11: z <= -16'd23;
        12: z <= -16'd40;
        13: z <= 16'd123;
        14: z <= -16'd50;
        15: z <= 16'd123;
      endcase
   end
endmodule

module layer_16_8_8_16_B_rom_4(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd27;
        1: z <= -16'd75;
      endcase
   end
endmodule

module layer_16_8_8_16_W_rom_5(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd57;
        1: z <= -16'd33;
        2: z <= -16'd104;
        3: z <= -16'd19;
        4: z <= 16'd107;
        5: z <= -16'd71;
        6: z <= -16'd70;
        7: z <= 16'd97;
        8: z <= 16'd72;
        9: z <= 16'd33;
        10: z <= 16'd10;
        11: z <= 16'd79;
        12: z <= -16'd83;
        13: z <= 16'd121;
        14: z <= -16'd109;
        15: z <= -16'd110;
      endcase
   end
endmodule

module layer_16_8_8_16_B_rom_5(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd117;
        1: z <= -16'd59;
      endcase
   end
endmodule

module layer_16_8_8_16_W_rom_6(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd67;
        1: z <= -16'd40;
        2: z <= -16'd103;
        3: z <= -16'd12;
        4: z <= 16'd106;
        5: z <= 16'd17;
        6: z <= 16'd22;
        7: z <= 16'd78;
        8: z <= 16'd2;
        9: z <= 16'd75;
        10: z <= 16'd98;
        11: z <= 16'd28;
        12: z <= -16'd103;
        13: z <= 16'd0;
        14: z <= -16'd124;
        15: z <= -16'd80;
      endcase
   end
endmodule

module layer_16_8_8_16_B_rom_6(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd50;
        1: z <= 16'd86;
      endcase
   end
endmodule

module layer_16_8_8_16_W_rom_7(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd50;
        1: z <= 16'd72;
        2: z <= 16'd33;
        3: z <= 16'd50;
        4: z <= 16'd102;
        5: z <= 16'd120;
        6: z <= -16'd69;
        7: z <= -16'd98;
        8: z <= 16'd63;
        9: z <= -16'd98;
        10: z <= -16'd82;
        11: z <= 16'd119;
        12: z <= -16'd13;
        13: z <= -16'd55;
        14: z <= -16'd40;
        15: z <= -16'd66;
      endcase
   end
endmodule

module layer_16_8_8_16_B_rom_7(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd29;
        1: z <= 16'd55;
      endcase
   end
endmodule

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
