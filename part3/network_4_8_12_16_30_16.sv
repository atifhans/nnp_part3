// ------------------------------------------//
// Neural Network -- Part 3                    
// ------------------------------------------//
// NAME:  Atif Iqbal                           
// NETID: aahangar                             
// SBUID: 111416569                            
// ------------------------------------------//


module network_4_8_12_16_30_16 #(
   parameter M1 = 8,
   parameter M2 = 12,
   parameter M3 = 16,
   parameter N  = 4,
   parameter P1 = 8,
   parameter P2 = 6,
   parameter P3 = 16,
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

   logic                   ms_ready_1;
   logic                   sm_valid_1;
   logic signed [T-1:0]    sm_data_out_1;
   logic                   ms_ready_2;
   logic                   sm_valid_2;
   logic signed [T-1:0]    sm_data_out_2;

   layer1_8_4_8_16 #(
      .M         ( 8         ),
      .N         ( 4          ),
      .P         ( 8         ),
      .T         ( 16       ))
   u_layer1_8_4_8_16 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .s_valid   ( s_valid              ),
      .data_in   ( data_in              ),
      .s_ready   ( s_ready              ),
      .m_ready   ( ms_ready_1           ),
      .m_valid   ( sm_valid_1           ),
      .data_out  ( sm_data_out_1        ));

   layer2_12_8_6_16 #(
      .M         ( 12         ),
      .N         ( 8         ),
      .P         ( 6         ),
      .T         ( 16       ))
   u_layer2_12_8_6_16 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .s_ready   ( ms_ready_1           ),
      .s_valid   ( sm_valid_1           ),
      .data_in   ( sm_data_out_1        ),
      .m_ready   ( ms_ready_2           ),
      .m_valid   ( sm_valid_2           ),
      .data_out  ( sm_data_out_2        ));

   layer3_16_12_16_16 #(
      .M         ( 16         ),
      .N         ( 12         ),
      .P         ( 16         ),
      .T         ( 16       ))
   u_layer3_16_12_16_16 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .s_ready   ( ms_ready_2           ),
      .s_valid   ( sm_valid_2           ),
      .data_in   ( sm_data_out_2        ),
      .m_ready   ( m_ready              ),
      .m_valid   ( m_valid              ),
      .data_out  ( data_out             ));

endmodule
module layer1_8_4_8_16 #(
   parameter M = 8,
   parameter N = 4,
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

layer1_8_4_8_16_W_rom_0 u_w_rom_0 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[0]      ),
       .z       ( rom_w_data_out[0]     ));

layer1_8_4_8_16_B_rom_0 u_b_rom_0 (
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

layer1_8_4_8_16_W_rom_1 u_w_rom_1 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[1]      ),
       .z       ( rom_w_data_out[1]     ));

layer1_8_4_8_16_B_rom_1 u_b_rom_1 (
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

layer1_8_4_8_16_W_rom_2 u_w_rom_2 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[2]      ),
       .z       ( rom_w_data_out[2]     ));

layer1_8_4_8_16_B_rom_2 u_b_rom_2 (
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

layer1_8_4_8_16_W_rom_3 u_w_rom_3 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[3]      ),
       .z       ( rom_w_data_out[3]     ));

layer1_8_4_8_16_B_rom_3 u_b_rom_3 (
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

layer1_8_4_8_16_W_rom_4 u_w_rom_4 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[4]      ),
       .z       ( rom_w_data_out[4]     ));

layer1_8_4_8_16_B_rom_4 u_b_rom_4 (
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

layer1_8_4_8_16_W_rom_5 u_w_rom_5 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[5]      ),
       .z       ( rom_w_data_out[5]     ));

layer1_8_4_8_16_B_rom_5 u_b_rom_5 (
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

layer1_8_4_8_16_W_rom_6 u_w_rom_6 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[6]      ),
       .z       ( rom_w_data_out[6]     ));

layer1_8_4_8_16_B_rom_6 u_b_rom_6 (
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

layer1_8_4_8_16_W_rom_7 u_w_rom_7 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[7]      ),
       .z       ( rom_w_data_out[7]     ));

layer1_8_4_8_16_B_rom_7 u_b_rom_7 (
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

module layer1_8_4_8_16_W_rom_0(clk, addr, z);
   input clk;
   input [1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd80;
        1: z <= 16'd68;
        2: z <= -16'd86;
        3: z <= -16'd128;
      endcase
   end
endmodule

module layer1_8_4_8_16_B_rom_0(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd85;
      endcase
   end
endmodule

module layer1_8_4_8_16_W_rom_1(clk, addr, z);
   input clk;
   input [1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd1;
        1: z <= -16'd10;
        2: z <= 16'd61;
        3: z <= 16'd84;
      endcase
   end
endmodule

module layer1_8_4_8_16_B_rom_1(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd110;
      endcase
   end
endmodule

module layer1_8_4_8_16_W_rom_2(clk, addr, z);
   input clk;
   input [1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd7;
        1: z <= -16'd101;
        2: z <= 16'd12;
        3: z <= -16'd53;
      endcase
   end
endmodule

module layer1_8_4_8_16_B_rom_2(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd122;
      endcase
   end
endmodule

module layer1_8_4_8_16_W_rom_3(clk, addr, z);
   input clk;
   input [1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd36;
        1: z <= 16'd34;
        2: z <= -16'd87;
        3: z <= -16'd68;
      endcase
   end
endmodule

module layer1_8_4_8_16_B_rom_3(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd45;
      endcase
   end
endmodule

module layer1_8_4_8_16_W_rom_4(clk, addr, z);
   input clk;
   input [1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd8;
        1: z <= 16'd34;
        2: z <= 16'd3;
        3: z <= 16'd110;
      endcase
   end
endmodule

module layer1_8_4_8_16_B_rom_4(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd28;
      endcase
   end
endmodule

module layer1_8_4_8_16_W_rom_5(clk, addr, z);
   input clk;
   input [1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd95;
        1: z <= -16'd58;
        2: z <= 16'd95;
        3: z <= 16'd11;
      endcase
   end
endmodule

module layer1_8_4_8_16_B_rom_5(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd56;
      endcase
   end
endmodule

module layer1_8_4_8_16_W_rom_6(clk, addr, z);
   input clk;
   input [1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd51;
        1: z <= -16'd35;
        2: z <= 16'd116;
        3: z <= -16'd20;
      endcase
   end
endmodule

module layer1_8_4_8_16_B_rom_6(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd1;
      endcase
   end
endmodule

module layer1_8_4_8_16_W_rom_7(clk, addr, z);
   input clk;
   input [1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd74;
        1: z <= -16'd25;
        2: z <= 16'd68;
        3: z <= 16'd122;
      endcase
   end
endmodule

module layer1_8_4_8_16_B_rom_7(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd107;
      endcase
   end
endmodule


module layer2_12_8_6_16 #(
   parameter M = 12,
   parameter N = 8,
   parameter P = 6,
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

layer2_12_8_6_16_W_rom_0 u_w_rom_0 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[0]      ),
       .z       ( rom_w_data_out[0]     ));

layer2_12_8_6_16_B_rom_0 u_b_rom_0 (
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

layer2_12_8_6_16_W_rom_1 u_w_rom_1 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[1]      ),
       .z       ( rom_w_data_out[1]     ));

layer2_12_8_6_16_B_rom_1 u_b_rom_1 (
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

layer2_12_8_6_16_W_rom_2 u_w_rom_2 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[2]      ),
       .z       ( rom_w_data_out[2]     ));

layer2_12_8_6_16_B_rom_2 u_b_rom_2 (
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

layer2_12_8_6_16_W_rom_3 u_w_rom_3 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[3]      ),
       .z       ( rom_w_data_out[3]     ));

layer2_12_8_6_16_B_rom_3 u_b_rom_3 (
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

layer2_12_8_6_16_W_rom_4 u_w_rom_4 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[4]      ),
       .z       ( rom_w_data_out[4]     ));

layer2_12_8_6_16_B_rom_4 u_b_rom_4 (
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

layer2_12_8_6_16_W_rom_5 u_w_rom_5 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[5]      ),
       .z       ( rom_w_data_out[5]     ));

layer2_12_8_6_16_B_rom_5 u_b_rom_5 (
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

module layer2_12_8_6_16_W_rom_0(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd83;
        1: z <= -16'd115;
        2: z <= -16'd74;
        3: z <= -16'd9;
        4: z <= 16'd47;
        5: z <= -16'd33;
        6: z <= 16'd51;
        7: z <= -16'd89;
        8: z <= -16'd5;
        9: z <= -16'd6;
        10: z <= -16'd2;
        11: z <= -16'd62;
        12: z <= -16'd12;
        13: z <= 16'd21;
        14: z <= -16'd75;
        15: z <= -16'd36;
      endcase
   end
endmodule

module layer2_12_8_6_16_B_rom_0(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd100;
        1: z <= 16'd50;
      endcase
   end
endmodule

module layer2_12_8_6_16_W_rom_1(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd127;
        1: z <= -16'd74;
        2: z <= -16'd106;
        3: z <= 16'd96;
        4: z <= -16'd4;
        5: z <= 16'd117;
        6: z <= -16'd21;
        7: z <= 16'd74;
        8: z <= 16'd38;
        9: z <= 16'd84;
        10: z <= 16'd41;
        11: z <= -16'd16;
        12: z <= 16'd117;
        13: z <= 16'd97;
        14: z <= -16'd116;
        15: z <= -16'd94;
      endcase
   end
endmodule

module layer2_12_8_6_16_B_rom_1(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd22;
        1: z <= 16'd54;
      endcase
   end
endmodule

module layer2_12_8_6_16_W_rom_2(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd46;
        1: z <= -16'd32;
        2: z <= -16'd74;
        3: z <= -16'd100;
        4: z <= 16'd71;
        5: z <= 16'd122;
        6: z <= -16'd105;
        7: z <= 16'd114;
        8: z <= -16'd49;
        9: z <= -16'd96;
        10: z <= 16'd127;
        11: z <= -16'd99;
        12: z <= 16'd104;
        13: z <= -16'd125;
        14: z <= 16'd109;
        15: z <= 16'd102;
      endcase
   end
endmodule

module layer2_12_8_6_16_B_rom_2(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd103;
        1: z <= -16'd15;
      endcase
   end
endmodule

module layer2_12_8_6_16_W_rom_3(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd104;
        1: z <= -16'd111;
        2: z <= 16'd31;
        3: z <= -16'd52;
        4: z <= 16'd73;
        5: z <= -16'd96;
        6: z <= -16'd72;
        7: z <= 16'd28;
        8: z <= -16'd99;
        9: z <= 16'd29;
        10: z <= -16'd32;
        11: z <= -16'd84;
        12: z <= 16'd56;
        13: z <= 16'd37;
        14: z <= 16'd13;
        15: z <= -16'd76;
      endcase
   end
endmodule

module layer2_12_8_6_16_B_rom_3(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd76;
        1: z <= -16'd110;
      endcase
   end
endmodule

module layer2_12_8_6_16_W_rom_4(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd83;
        1: z <= -16'd18;
        2: z <= -16'd108;
        3: z <= 16'd92;
        4: z <= 16'd78;
        5: z <= 16'd71;
        6: z <= -16'd124;
        7: z <= 16'd79;
        8: z <= -16'd97;
        9: z <= -16'd117;
        10: z <= -16'd10;
        11: z <= 16'd20;
        12: z <= 16'd32;
        13: z <= 16'd43;
        14: z <= 16'd112;
        15: z <= -16'd57;
      endcase
   end
endmodule

module layer2_12_8_6_16_B_rom_4(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd25;
        1: z <= 16'd98;
      endcase
   end
endmodule

module layer2_12_8_6_16_W_rom_5(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd126;
        1: z <= -16'd102;
        2: z <= 16'd48;
        3: z <= -16'd6;
        4: z <= -16'd113;
        5: z <= -16'd101;
        6: z <= -16'd60;
        7: z <= -16'd31;
        8: z <= 16'd0;
        9: z <= 16'd25;
        10: z <= 16'd55;
        11: z <= -16'd11;
        12: z <= -16'd6;
        13: z <= 16'd67;
        14: z <= 16'd23;
        15: z <= 16'd74;
      endcase
   end
endmodule

module layer2_12_8_6_16_B_rom_5(clk, addr, z);
   input clk;
   input [0:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd84;
        1: z <= -16'd86;
      endcase
   end
endmodule


module layer3_16_12_16_16 #(
   parameter M = 16,
   parameter N = 12,
   parameter P = 16,
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

layer3_16_12_16_16_W_rom_0 u_w_rom_0 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[0]      ),
       .z       ( rom_w_data_out[0]     ));

layer3_16_12_16_16_B_rom_0 u_b_rom_0 (
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

layer3_16_12_16_16_W_rom_1 u_w_rom_1 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[1]      ),
       .z       ( rom_w_data_out[1]     ));

layer3_16_12_16_16_B_rom_1 u_b_rom_1 (
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

layer3_16_12_16_16_W_rom_2 u_w_rom_2 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[2]      ),
       .z       ( rom_w_data_out[2]     ));

layer3_16_12_16_16_B_rom_2 u_b_rom_2 (
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

layer3_16_12_16_16_W_rom_3 u_w_rom_3 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[3]      ),
       .z       ( rom_w_data_out[3]     ));

layer3_16_12_16_16_B_rom_3 u_b_rom_3 (
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

layer3_16_12_16_16_W_rom_4 u_w_rom_4 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[4]      ),
       .z       ( rom_w_data_out[4]     ));

layer3_16_12_16_16_B_rom_4 u_b_rom_4 (
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

layer3_16_12_16_16_W_rom_5 u_w_rom_5 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[5]      ),
       .z       ( rom_w_data_out[5]     ));

layer3_16_12_16_16_B_rom_5 u_b_rom_5 (
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

layer3_16_12_16_16_W_rom_6 u_w_rom_6 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[6]      ),
       .z       ( rom_w_data_out[6]     ));

layer3_16_12_16_16_B_rom_6 u_b_rom_6 (
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

layer3_16_12_16_16_W_rom_7 u_w_rom_7 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[7]      ),
       .z       ( rom_w_data_out[7]     ));

layer3_16_12_16_16_B_rom_7 u_b_rom_7 (
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

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_8 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[8]     ),
      .addr     ( ram_x_addr[8]         ),
      .wr_en    ( ram_x_wr_en           ));

layer3_16_12_16_16_W_rom_8 u_w_rom_8 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[8]      ),
       .z       ( rom_w_data_out[8]     ));

layer3_16_12_16_16_B_rom_8 u_b_rom_8 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[8]      ),
       .z       ( rom_b_data_out[8]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_8 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[8]    ),
      .b         ( rom_b_data_out[8]    ),
      .x         ( ram_x_data_out[8]    ),
      .valid_in  ( mac_valid_in[8]      ),
      .f         ( mac_data_out[8]      ),
      .valid_out ( mac_valid_out[8]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_9 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[9]     ),
      .addr     ( ram_x_addr[9]         ),
      .wr_en    ( ram_x_wr_en           ));

layer3_16_12_16_16_W_rom_9 u_w_rom_9 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[9]      ),
       .z       ( rom_w_data_out[9]     ));

layer3_16_12_16_16_B_rom_9 u_b_rom_9 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[9]      ),
       .z       ( rom_b_data_out[9]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_9 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[9]    ),
      .b         ( rom_b_data_out[9]    ),
      .x         ( ram_x_data_out[9]    ),
      .valid_in  ( mac_valid_in[9]      ),
      .f         ( mac_data_out[9]      ),
      .valid_out ( mac_valid_out[9]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_10 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[10]     ),
      .addr     ( ram_x_addr[10]         ),
      .wr_en    ( ram_x_wr_en           ));

layer3_16_12_16_16_W_rom_10 u_w_rom_10 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[10]      ),
       .z       ( rom_w_data_out[10]     ));

layer3_16_12_16_16_B_rom_10 u_b_rom_10 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[10]      ),
       .z       ( rom_b_data_out[10]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_10 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[10]    ),
      .b         ( rom_b_data_out[10]    ),
      .x         ( ram_x_data_out[10]    ),
      .valid_in  ( mac_valid_in[10]      ),
      .f         ( mac_data_out[10]      ),
      .valid_out ( mac_valid_out[10]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_11 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[11]     ),
      .addr     ( ram_x_addr[11]         ),
      .wr_en    ( ram_x_wr_en           ));

layer3_16_12_16_16_W_rom_11 u_w_rom_11 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[11]      ),
       .z       ( rom_w_data_out[11]     ));

layer3_16_12_16_16_B_rom_11 u_b_rom_11 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[11]      ),
       .z       ( rom_b_data_out[11]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_11 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[11]    ),
      .b         ( rom_b_data_out[11]    ),
      .x         ( ram_x_data_out[11]    ),
      .valid_in  ( mac_valid_in[11]      ),
      .f         ( mac_data_out[11]      ),
      .valid_out ( mac_valid_out[11]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_12 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[12]     ),
      .addr     ( ram_x_addr[12]         ),
      .wr_en    ( ram_x_wr_en           ));

layer3_16_12_16_16_W_rom_12 u_w_rom_12 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[12]      ),
       .z       ( rom_w_data_out[12]     ));

layer3_16_12_16_16_B_rom_12 u_b_rom_12 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[12]      ),
       .z       ( rom_b_data_out[12]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_12 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[12]    ),
      .b         ( rom_b_data_out[12]    ),
      .x         ( ram_x_data_out[12]    ),
      .valid_in  ( mac_valid_in[12]      ),
      .f         ( mac_data_out[12]      ),
      .valid_out ( mac_valid_out[12]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_13 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[13]     ),
      .addr     ( ram_x_addr[13]         ),
      .wr_en    ( ram_x_wr_en           ));

layer3_16_12_16_16_W_rom_13 u_w_rom_13 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[13]      ),
       .z       ( rom_w_data_out[13]     ));

layer3_16_12_16_16_B_rom_13 u_b_rom_13 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[13]      ),
       .z       ( rom_b_data_out[13]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_13 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[13]    ),
      .b         ( rom_b_data_out[13]    ),
      .x         ( ram_x_data_out[13]    ),
      .valid_in  ( mac_valid_in[13]      ),
      .f         ( mac_data_out[13]      ),
      .valid_out ( mac_valid_out[13]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_14 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[14]     ),
      .addr     ( ram_x_addr[14]         ),
      .wr_en    ( ram_x_wr_en           ));

layer3_16_12_16_16_W_rom_14 u_w_rom_14 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[14]      ),
       .z       ( rom_w_data_out[14]     ));

layer3_16_12_16_16_B_rom_14 u_b_rom_14 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[14]      ),
       .z       ( rom_b_data_out[14]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_14 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[14]    ),
      .b         ( rom_b_data_out[14]    ),
      .x         ( ram_x_data_out[14]    ),
      .valid_in  ( mac_valid_in[14]      ),
      .f         ( mac_data_out[14]      ),
      .valid_out ( mac_valid_out[14]     ),
      .overflow  ( /* Not Used */       ));

   memory #(
      .WIDTH    ( T                     ),
      .SIZE     ( VEC_x_SIZE            ),
      .LOGSIZE  ( VEC_x_ADDW            ))
   u_vec_x_mem_15 (
      .clk      ( clk                   ),
      .data_in  ( data_in               ),
      .data_out ( ram_x_data_out[15]     ),
      .addr     ( ram_x_addr[15]         ),
      .wr_en    ( ram_x_wr_en           ));

layer3_16_12_16_16_W_rom_15 u_w_rom_15 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[15]      ),
       .z       ( rom_w_data_out[15]     ));

layer3_16_12_16_16_B_rom_15 u_b_rom_15 (
       .clk     ( clk                   ),
       .addr    ( rom_b_rd_addr[15]      ),
       .z       ( rom_b_data_out[15]     ));

   part3_mac #(
      .T         ( T                    ),
      .NUM_S     ( 1                    ),
      .VEC_S     ( VEC_x_SIZE           ))
   u_mac_15 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .a         ( rom_w_data_out[15]    ),
      .b         ( rom_b_data_out[15]    ),
      .x         ( ram_x_data_out[15]    ),
      .valid_in  ( mac_valid_in[15]      ),
      .f         ( mac_data_out[15]      ),
      .valid_out ( mac_valid_out[15]     ),
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
         rom_w_rd_addr[8] <= 'd0;
         rom_b_rd_addr[8] <= 'd0;
         ram_x_rd_addr[8] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[8] == MAT_W_SIZE-1 && next_req[8]) begin
            ram_x_rd_addr[8] <= 'd0;
            rom_w_rd_addr[8] <= 'd0;
            rom_b_rd_addr[8] <= 'd0;
         end
         else if (ram_x_rd_addr[ 8] == VEC_x_SIZE-1 && next_req[8]) begin
            ram_x_rd_addr[8] <= 'd0;
            rom_w_rd_addr[8] <= rom_w_rd_addr[8] + 1'b1;
            rom_b_rd_addr[8] <= rom_b_rd_addr[8] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[8] < VEC_x_SIZE && next_req[8]) begin
            ram_x_rd_addr[8] <= ram_x_rd_addr[8] + 1'b1;
            rom_w_rd_addr[8] <= rom_w_rd_addr[8] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[9] <= 'd0;
         rom_b_rd_addr[9] <= 'd0;
         ram_x_rd_addr[9] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[9] == MAT_W_SIZE-1 && next_req[9]) begin
            ram_x_rd_addr[9] <= 'd0;
            rom_w_rd_addr[9] <= 'd0;
            rom_b_rd_addr[9] <= 'd0;
         end
         else if (ram_x_rd_addr[ 9] == VEC_x_SIZE-1 && next_req[9]) begin
            ram_x_rd_addr[9] <= 'd0;
            rom_w_rd_addr[9] <= rom_w_rd_addr[9] + 1'b1;
            rom_b_rd_addr[9] <= rom_b_rd_addr[9] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[9] < VEC_x_SIZE && next_req[9]) begin
            ram_x_rd_addr[9] <= ram_x_rd_addr[9] + 1'b1;
            rom_w_rd_addr[9] <= rom_w_rd_addr[9] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[10] <= 'd0;
         rom_b_rd_addr[10] <= 'd0;
         ram_x_rd_addr[10] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[10] == MAT_W_SIZE-1 && next_req[10]) begin
            ram_x_rd_addr[10] <= 'd0;
            rom_w_rd_addr[10] <= 'd0;
            rom_b_rd_addr[10] <= 'd0;
         end
         else if (ram_x_rd_addr[ 10] == VEC_x_SIZE-1 && next_req[10]) begin
            ram_x_rd_addr[10] <= 'd0;
            rom_w_rd_addr[10] <= rom_w_rd_addr[10] + 1'b1;
            rom_b_rd_addr[10] <= rom_b_rd_addr[10] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[10] < VEC_x_SIZE && next_req[10]) begin
            ram_x_rd_addr[10] <= ram_x_rd_addr[10] + 1'b1;
            rom_w_rd_addr[10] <= rom_w_rd_addr[10] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[11] <= 'd0;
         rom_b_rd_addr[11] <= 'd0;
         ram_x_rd_addr[11] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[11] == MAT_W_SIZE-1 && next_req[11]) begin
            ram_x_rd_addr[11] <= 'd0;
            rom_w_rd_addr[11] <= 'd0;
            rom_b_rd_addr[11] <= 'd0;
         end
         else if (ram_x_rd_addr[ 11] == VEC_x_SIZE-1 && next_req[11]) begin
            ram_x_rd_addr[11] <= 'd0;
            rom_w_rd_addr[11] <= rom_w_rd_addr[11] + 1'b1;
            rom_b_rd_addr[11] <= rom_b_rd_addr[11] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[11] < VEC_x_SIZE && next_req[11]) begin
            ram_x_rd_addr[11] <= ram_x_rd_addr[11] + 1'b1;
            rom_w_rd_addr[11] <= rom_w_rd_addr[11] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[12] <= 'd0;
         rom_b_rd_addr[12] <= 'd0;
         ram_x_rd_addr[12] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[12] == MAT_W_SIZE-1 && next_req[12]) begin
            ram_x_rd_addr[12] <= 'd0;
            rom_w_rd_addr[12] <= 'd0;
            rom_b_rd_addr[12] <= 'd0;
         end
         else if (ram_x_rd_addr[ 12] == VEC_x_SIZE-1 && next_req[12]) begin
            ram_x_rd_addr[12] <= 'd0;
            rom_w_rd_addr[12] <= rom_w_rd_addr[12] + 1'b1;
            rom_b_rd_addr[12] <= rom_b_rd_addr[12] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[12] < VEC_x_SIZE && next_req[12]) begin
            ram_x_rd_addr[12] <= ram_x_rd_addr[12] + 1'b1;
            rom_w_rd_addr[12] <= rom_w_rd_addr[12] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[13] <= 'd0;
         rom_b_rd_addr[13] <= 'd0;
         ram_x_rd_addr[13] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[13] == MAT_W_SIZE-1 && next_req[13]) begin
            ram_x_rd_addr[13] <= 'd0;
            rom_w_rd_addr[13] <= 'd0;
            rom_b_rd_addr[13] <= 'd0;
         end
         else if (ram_x_rd_addr[ 13] == VEC_x_SIZE-1 && next_req[13]) begin
            ram_x_rd_addr[13] <= 'd0;
            rom_w_rd_addr[13] <= rom_w_rd_addr[13] + 1'b1;
            rom_b_rd_addr[13] <= rom_b_rd_addr[13] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[13] < VEC_x_SIZE && next_req[13]) begin
            ram_x_rd_addr[13] <= ram_x_rd_addr[13] + 1'b1;
            rom_w_rd_addr[13] <= rom_w_rd_addr[13] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[14] <= 'd0;
         rom_b_rd_addr[14] <= 'd0;
         ram_x_rd_addr[14] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[14] == MAT_W_SIZE-1 && next_req[14]) begin
            ram_x_rd_addr[14] <= 'd0;
            rom_w_rd_addr[14] <= 'd0;
            rom_b_rd_addr[14] <= 'd0;
         end
         else if (ram_x_rd_addr[ 14] == VEC_x_SIZE-1 && next_req[14]) begin
            ram_x_rd_addr[14] <= 'd0;
            rom_w_rd_addr[14] <= rom_w_rd_addr[14] + 1'b1;
            rom_b_rd_addr[14] <= rom_b_rd_addr[14] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[14] < VEC_x_SIZE && next_req[14]) begin
            ram_x_rd_addr[14] <= ram_x_rd_addr[14] + 1'b1;
            rom_w_rd_addr[14] <= rom_w_rd_addr[14] + 1'd1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         rom_w_rd_addr[15] <= 'd0;
         rom_b_rd_addr[15] <= 'd0;
         ram_x_rd_addr[15] <= 'd0;
      end
      else begin
         if(rom_w_rd_addr[15] == MAT_W_SIZE-1 && next_req[15]) begin
            ram_x_rd_addr[15] <= 'd0;
            rom_w_rd_addr[15] <= 'd0;
            rom_b_rd_addr[15] <= 'd0;
         end
         else if (ram_x_rd_addr[ 15] == VEC_x_SIZE-1 && next_req[15]) begin
            ram_x_rd_addr[15] <= 'd0;
            rom_w_rd_addr[15] <= rom_w_rd_addr[15] + 1'b1;
            rom_b_rd_addr[15] <= rom_b_rd_addr[15] + 1'b1;
         end
         else if ((state == COMPUTE_y) && vec_cnt[15] < VEC_x_SIZE && next_req[15]) begin
            ram_x_rd_addr[15] <= ram_x_rd_addr[15] + 1'b1;
            rom_w_rd_addr[15] <= rom_w_rd_addr[15] + 1'd1;
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
         next_req[8]     <= 1'b1;
         mac_valid_in[8] <= 1'b0;
         vec_cnt[8]      <=  'd0;
         vld_in_cnt[8]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[8]   <=  'd0;
            next_req[8]     <= 1'b1;
         end
         else if(vld_in_cnt[8] == VEC_b_SIZE) begin
            next_req[8]      <= 1'b0;
            mac_valid_in[8]  <= 1'b0;
            vec_cnt[8]       <= 2'd0;
         end
         else if(vec_cnt[8] == VEC_x_SIZE) begin
            next_req[8]      <= 1'b0;
            mac_valid_in[8]  <= 1'b0;
            vec_cnt[8]       <= 2'd0;
            vld_in_cnt[8]    <= vld_in_cnt[8] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 8) && (vld_in_cnt[8] < VEC_b_SIZE)) begin
            next_req[8]      <= 1'b1;
         end
         else if (next_req[8] && (state == COMPUTE_y)) begin
            next_req[8]      <= 1'b1;
            mac_valid_in[8]  <= 1'b1;
            vec_cnt[8]       <= vec_cnt[8] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[9]     <= 1'b1;
         mac_valid_in[9] <= 1'b0;
         vec_cnt[9]      <=  'd0;
         vld_in_cnt[9]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[9]   <=  'd0;
            next_req[9]     <= 1'b1;
         end
         else if(vld_in_cnt[9] == VEC_b_SIZE) begin
            next_req[9]      <= 1'b0;
            mac_valid_in[9]  <= 1'b0;
            vec_cnt[9]       <= 2'd0;
         end
         else if(vec_cnt[9] == VEC_x_SIZE) begin
            next_req[9]      <= 1'b0;
            mac_valid_in[9]  <= 1'b0;
            vec_cnt[9]       <= 2'd0;
            vld_in_cnt[9]    <= vld_in_cnt[9] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 9) && (vld_in_cnt[9] < VEC_b_SIZE)) begin
            next_req[9]      <= 1'b1;
         end
         else if (next_req[9] && (state == COMPUTE_y)) begin
            next_req[9]      <= 1'b1;
            mac_valid_in[9]  <= 1'b1;
            vec_cnt[9]       <= vec_cnt[9] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[10]     <= 1'b1;
         mac_valid_in[10] <= 1'b0;
         vec_cnt[10]      <=  'd0;
         vld_in_cnt[10]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[10]   <=  'd0;
            next_req[10]     <= 1'b1;
         end
         else if(vld_in_cnt[10] == VEC_b_SIZE) begin
            next_req[10]      <= 1'b0;
            mac_valid_in[10]  <= 1'b0;
            vec_cnt[10]       <= 2'd0;
         end
         else if(vec_cnt[10] == VEC_x_SIZE) begin
            next_req[10]      <= 1'b0;
            mac_valid_in[10]  <= 1'b0;
            vec_cnt[10]       <= 2'd0;
            vld_in_cnt[10]    <= vld_in_cnt[10] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 10) && (vld_in_cnt[10] < VEC_b_SIZE)) begin
            next_req[10]      <= 1'b1;
         end
         else if (next_req[10] && (state == COMPUTE_y)) begin
            next_req[10]      <= 1'b1;
            mac_valid_in[10]  <= 1'b1;
            vec_cnt[10]       <= vec_cnt[10] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[11]     <= 1'b1;
         mac_valid_in[11] <= 1'b0;
         vec_cnt[11]      <=  'd0;
         vld_in_cnt[11]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[11]   <=  'd0;
            next_req[11]     <= 1'b1;
         end
         else if(vld_in_cnt[11] == VEC_b_SIZE) begin
            next_req[11]      <= 1'b0;
            mac_valid_in[11]  <= 1'b0;
            vec_cnt[11]       <= 2'd0;
         end
         else if(vec_cnt[11] == VEC_x_SIZE) begin
            next_req[11]      <= 1'b0;
            mac_valid_in[11]  <= 1'b0;
            vec_cnt[11]       <= 2'd0;
            vld_in_cnt[11]    <= vld_in_cnt[11] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 11) && (vld_in_cnt[11] < VEC_b_SIZE)) begin
            next_req[11]      <= 1'b1;
         end
         else if (next_req[11] && (state == COMPUTE_y)) begin
            next_req[11]      <= 1'b1;
            mac_valid_in[11]  <= 1'b1;
            vec_cnt[11]       <= vec_cnt[11] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[12]     <= 1'b1;
         mac_valid_in[12] <= 1'b0;
         vec_cnt[12]      <=  'd0;
         vld_in_cnt[12]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[12]   <=  'd0;
            next_req[12]     <= 1'b1;
         end
         else if(vld_in_cnt[12] == VEC_b_SIZE) begin
            next_req[12]      <= 1'b0;
            mac_valid_in[12]  <= 1'b0;
            vec_cnt[12]       <= 2'd0;
         end
         else if(vec_cnt[12] == VEC_x_SIZE) begin
            next_req[12]      <= 1'b0;
            mac_valid_in[12]  <= 1'b0;
            vec_cnt[12]       <= 2'd0;
            vld_in_cnt[12]    <= vld_in_cnt[12] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 12) && (vld_in_cnt[12] < VEC_b_SIZE)) begin
            next_req[12]      <= 1'b1;
         end
         else if (next_req[12] && (state == COMPUTE_y)) begin
            next_req[12]      <= 1'b1;
            mac_valid_in[12]  <= 1'b1;
            vec_cnt[12]       <= vec_cnt[12] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[13]     <= 1'b1;
         mac_valid_in[13] <= 1'b0;
         vec_cnt[13]      <=  'd0;
         vld_in_cnt[13]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[13]   <=  'd0;
            next_req[13]     <= 1'b1;
         end
         else if(vld_in_cnt[13] == VEC_b_SIZE) begin
            next_req[13]      <= 1'b0;
            mac_valid_in[13]  <= 1'b0;
            vec_cnt[13]       <= 2'd0;
         end
         else if(vec_cnt[13] == VEC_x_SIZE) begin
            next_req[13]      <= 1'b0;
            mac_valid_in[13]  <= 1'b0;
            vec_cnt[13]       <= 2'd0;
            vld_in_cnt[13]    <= vld_in_cnt[13] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 13) && (vld_in_cnt[13] < VEC_b_SIZE)) begin
            next_req[13]      <= 1'b1;
         end
         else if (next_req[13] && (state == COMPUTE_y)) begin
            next_req[13]      <= 1'b1;
            mac_valid_in[13]  <= 1'b1;
            vec_cnt[13]       <= vec_cnt[13] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[14]     <= 1'b1;
         mac_valid_in[14] <= 1'b0;
         vec_cnt[14]      <=  'd0;
         vld_in_cnt[14]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[14]   <=  'd0;
            next_req[14]     <= 1'b1;
         end
         else if(vld_in_cnt[14] == VEC_b_SIZE) begin
            next_req[14]      <= 1'b0;
            mac_valid_in[14]  <= 1'b0;
            vec_cnt[14]       <= 2'd0;
         end
         else if(vec_cnt[14] == VEC_x_SIZE) begin
            next_req[14]      <= 1'b0;
            mac_valid_in[14]  <= 1'b0;
            vec_cnt[14]       <= 2'd0;
            vld_in_cnt[14]    <= vld_in_cnt[14] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 14) && (vld_in_cnt[14] < VEC_b_SIZE)) begin
            next_req[14]      <= 1'b1;
         end
         else if (next_req[14] && (state == COMPUTE_y)) begin
            next_req[14]      <= 1'b1;
            mac_valid_in[14]  <= 1'b1;
            vec_cnt[14]       <= vec_cnt[14] + 1'b1;
         end
      end

   always_ff @(posedge clk)
      if(reset) begin
         next_req[15]     <= 1'b1;
         mac_valid_in[15] <= 1'b0;
         vec_cnt[15]      <=  'd0;
         vld_in_cnt[15]   <=  'd0;
      end
      else begin
         if(compute_done) begin
            vld_in_cnt[15]   <=  'd0;
            next_req[15]     <= 1'b1;
         end
         else if(vld_in_cnt[15] == VEC_b_SIZE) begin
            next_req[15]      <= 1'b0;
            mac_valid_in[15]  <= 1'b0;
            vec_cnt[15]       <= 2'd0;
         end
         else if(vec_cnt[15] == VEC_x_SIZE) begin
            next_req[15]      <= 1'b0;
            mac_valid_in[15]  <= 1'b0;
            vec_cnt[15]       <= 2'd0;
            vld_in_cnt[15]    <= vld_in_cnt[15] + 1'b1;
         end
         else if (m_valid && m_ready && (vld_out_cnt == 15) && (vld_in_cnt[15] < VEC_b_SIZE)) begin
            next_req[15]      <= 1'b1;
         end
         else if (next_req[15] && (state == COMPUTE_y)) begin
            next_req[15]      <= 1'b1;
            mac_valid_in[15]  <= 1'b1;
            vec_cnt[15]       <= vec_cnt[15] + 1'b1;
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

module layer3_16_12_16_16_W_rom_0(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd55;
        1: z <= -16'd16;
        2: z <= -16'd34;
        3: z <= 16'd87;
        4: z <= -16'd5;
        5: z <= 16'd84;
        6: z <= -16'd21;
        7: z <= -16'd100;
        8: z <= 16'd0;
        9: z <= -16'd37;
        10: z <= -16'd29;
        11: z <= -16'd128;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_0(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd87;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_1(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd117;
        1: z <= -16'd102;
        2: z <= -16'd11;
        3: z <= -16'd17;
        4: z <= 16'd93;
        5: z <= -16'd116;
        6: z <= -16'd71;
        7: z <= 16'd65;
        8: z <= 16'd35;
        9: z <= -16'd95;
        10: z <= 16'd13;
        11: z <= -16'd68;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_1(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd67;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_2(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd117;
        1: z <= -16'd65;
        2: z <= 16'd115;
        3: z <= -16'd25;
        4: z <= -16'd46;
        5: z <= 16'd85;
        6: z <= 16'd17;
        7: z <= -16'd119;
        8: z <= -16'd59;
        9: z <= 16'd111;
        10: z <= 16'd96;
        11: z <= 16'd65;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_2(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd63;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_3(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd67;
        1: z <= -16'd53;
        2: z <= 16'd93;
        3: z <= -16'd61;
        4: z <= 16'd39;
        5: z <= -16'd64;
        6: z <= -16'd61;
        7: z <= 16'd28;
        8: z <= -16'd38;
        9: z <= 16'd56;
        10: z <= -16'd117;
        11: z <= -16'd73;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_3(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd86;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_4(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd69;
        1: z <= -16'd59;
        2: z <= 16'd121;
        3: z <= -16'd24;
        4: z <= -16'd26;
        5: z <= 16'd6;
        6: z <= 16'd36;
        7: z <= -16'd37;
        8: z <= 16'd70;
        9: z <= 16'd23;
        10: z <= 16'd66;
        11: z <= -16'd104;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_4(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd110;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_5(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd19;
        1: z <= -16'd45;
        2: z <= -16'd95;
        3: z <= 16'd50;
        4: z <= -16'd62;
        5: z <= -16'd126;
        6: z <= -16'd13;
        7: z <= -16'd122;
        8: z <= -16'd51;
        9: z <= -16'd48;
        10: z <= -16'd55;
        11: z <= 16'd116;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_5(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd72;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_6(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd16;
        1: z <= 16'd13;
        2: z <= 16'd16;
        3: z <= 16'd106;
        4: z <= -16'd59;
        5: z <= 16'd28;
        6: z <= -16'd94;
        7: z <= -16'd118;
        8: z <= 16'd97;
        9: z <= -16'd101;
        10: z <= -16'd14;
        11: z <= -16'd57;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_6(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd94;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_7(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd33;
        1: z <= -16'd105;
        2: z <= 16'd34;
        3: z <= -16'd25;
        4: z <= 16'd46;
        5: z <= -16'd27;
        6: z <= -16'd1;
        7: z <= -16'd101;
        8: z <= 16'd56;
        9: z <= 16'd33;
        10: z <= 16'd78;
        11: z <= 16'd123;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_7(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd106;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_8(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd35;
        1: z <= -16'd63;
        2: z <= -16'd127;
        3: z <= 16'd112;
        4: z <= 16'd18;
        5: z <= -16'd54;
        6: z <= 16'd101;
        7: z <= -16'd94;
        8: z <= 16'd87;
        9: z <= -16'd11;
        10: z <= -16'd115;
        11: z <= -16'd99;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_8(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd28;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_9(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd111;
        1: z <= -16'd81;
        2: z <= -16'd89;
        3: z <= 16'd114;
        4: z <= -16'd54;
        5: z <= 16'd26;
        6: z <= -16'd71;
        7: z <= 16'd107;
        8: z <= 16'd49;
        9: z <= 16'd92;
        10: z <= -16'd45;
        11: z <= -16'd33;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_9(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd122;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_10(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd63;
        1: z <= 16'd82;
        2: z <= -16'd5;
        3: z <= 16'd121;
        4: z <= -16'd13;
        5: z <= -16'd55;
        6: z <= 16'd116;
        7: z <= -16'd106;
        8: z <= 16'd10;
        9: z <= 16'd117;
        10: z <= -16'd121;
        11: z <= -16'd100;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_10(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd30;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_11(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd64;
        1: z <= 16'd108;
        2: z <= -16'd65;
        3: z <= -16'd105;
        4: z <= -16'd31;
        5: z <= -16'd52;
        6: z <= -16'd76;
        7: z <= -16'd13;
        8: z <= -16'd5;
        9: z <= -16'd36;
        10: z <= -16'd27;
        11: z <= 16'd69;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_11(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd107;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_12(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd118;
        1: z <= 16'd31;
        2: z <= 16'd48;
        3: z <= 16'd39;
        4: z <= -16'd5;
        5: z <= -16'd125;
        6: z <= -16'd122;
        7: z <= 16'd60;
        8: z <= 16'd86;
        9: z <= 16'd1;
        10: z <= 16'd53;
        11: z <= -16'd55;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_12(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd19;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_13(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd74;
        1: z <= 16'd42;
        2: z <= -16'd32;
        3: z <= -16'd43;
        4: z <= 16'd31;
        5: z <= -16'd25;
        6: z <= -16'd15;
        7: z <= 16'd95;
        8: z <= -16'd45;
        9: z <= 16'd48;
        10: z <= 16'd119;
        11: z <= 16'd52;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_13(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd30;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_14(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd124;
        1: z <= -16'd85;
        2: z <= -16'd89;
        3: z <= -16'd9;
        4: z <= 16'd7;
        5: z <= 16'd13;
        6: z <= -16'd68;
        7: z <= -16'd3;
        8: z <= -16'd84;
        9: z <= 16'd109;
        10: z <= -16'd92;
        11: z <= 16'd39;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_14(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd23;
      endcase
   end
endmodule

module layer3_16_12_16_16_W_rom_15(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd112;
        1: z <= -16'd85;
        2: z <= -16'd29;
        3: z <= 16'd70;
        4: z <= 16'd44;
        5: z <= -16'd104;
        6: z <= -16'd112;
        7: z <= -16'd9;
        8: z <= 16'd66;
        9: z <= -16'd16;
        10: z <= 16'd76;
        11: z <= -16'd30;
      endcase
   end
endmodule

module layer3_16_12_16_16_B_rom_15(clk, addr, z);
   input clk;
   input [-1:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd63;
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
