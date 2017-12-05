// ------------------------------------------//
// Neural Network -- Part 3                    
// ------------------------------------------//
// NAME:  Atif Iqbal                           
// NETID: aahangar                             
// SBUID: 111416569                            
// ------------------------------------------//


module network_4_8_12_16_3_16 #(
   parameter M1 = 8,
   parameter M2 = 12,
   parameter M3 = 16,
   parameter N  = 4,
   parameter P1 = 1,
   parameter P2 = 1,
   parameter P3 = 1,
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

   layer1_8_4_1_16 #(
      .M         ( 8         ),
      .N         ( 4          ),
      .P         ( 1         ),
      .T         ( 16       ))
   u_layer1_8_4_1_16 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .s_valid   ( s_valid              ),
      .data_in   ( data_in              ),
      .s_ready   ( s_ready              ),
      .m_ready   ( ms_ready_1           ),
      .m_valid   ( sm_valid_1           ),
      .data_out  ( sm_data_out_1        ));

   layer2_12_8_1_16 #(
      .M         ( 12         ),
      .N         ( 8         ),
      .P         ( 1         ),
      .T         ( 16       ))
   u_layer2_12_8_1_16 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .s_ready   ( ms_ready_1           ),
      .s_valid   ( sm_valid_1           ),
      .data_in   ( sm_data_out_1        ),
      .m_ready   ( ms_ready_2           ),
      .m_valid   ( sm_valid_2           ),
      .data_out  ( sm_data_out_2        ));

   layer3_16_12_1_16 #(
      .M         ( 16         ),
      .N         ( 12         ),
      .P         ( 1         ),
      .T         ( 16       ))
   u_layer3_16_12_1_16 (
      .clk       ( clk                  ),
      .reset     ( reset                ),
      .s_ready   ( ms_ready_2           ),
      .s_valid   ( sm_valid_2           ),
      .data_in   ( sm_data_out_2        ),
      .m_ready   ( m_ready              ),
      .m_valid   ( m_valid              ),
      .data_out  ( data_out             ));

endmodule
module layer1_8_4_1_16 #(
   parameter M = 8,
   parameter N = 4,
   parameter P = 1,
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

layer1_8_4_1_16_W_rom_0 u_w_rom_0 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[0]      ),
       .z       ( rom_w_data_out[0]     ));

layer1_8_4_1_16_B_rom_0 u_b_rom_0 (
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

module layer1_8_4_1_16_W_rom_0(clk, addr, z);
   input clk;
   input [4:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd116;
        1: z <= 16'd22;
        2: z <= 16'd40;
        3: z <= -16'd125;
        4: z <= -16'd56;
        5: z <= 16'd35;
        6: z <= -16'd93;
        7: z <= 16'd13;
        8: z <= -16'd8;
        9: z <= -16'd117;
        10: z <= -16'd63;
        11: z <= -16'd90;
        12: z <= 16'd113;
        13: z <= 16'd110;
        14: z <= 16'd68;
        15: z <= -16'd22;
        16: z <= -16'd83;
        17: z <= -16'd103;
        18: z <= -16'd111;
        19: z <= 16'd38;
        20: z <= -16'd6;
        21: z <= 16'd63;
        22: z <= 16'd118;
        23: z <= -16'd25;
        24: z <= -16'd10;
        25: z <= 16'd0;
        26: z <= -16'd104;
        27: z <= -16'd105;
        28: z <= -16'd69;
        29: z <= 16'd52;
        30: z <= -16'd2;
        31: z <= -16'd81;
      endcase
   end
endmodule

module layer1_8_4_1_16_B_rom_0(clk, addr, z);
   input clk;
   input [2:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd54;
        1: z <= -16'd89;
        2: z <= -16'd78;
        3: z <= 16'd18;
        4: z <= 16'd74;
        5: z <= -16'd42;
        6: z <= -16'd96;
        7: z <= -16'd62;
      endcase
   end
endmodule


module layer2_12_8_1_16 #(
   parameter M = 12,
   parameter N = 8,
   parameter P = 1,
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

layer2_12_8_1_16_W_rom_0 u_w_rom_0 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[0]      ),
       .z       ( rom_w_data_out[0]     ));

layer2_12_8_1_16_B_rom_0 u_b_rom_0 (
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

module layer2_12_8_1_16_W_rom_0(clk, addr, z);
   input clk;
   input [6:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd31;
        1: z <= -16'd31;
        2: z <= -16'd24;
        3: z <= -16'd46;
        4: z <= -16'd49;
        5: z <= -16'd83;
        6: z <= 16'd60;
        7: z <= -16'd4;
        8: z <= -16'd58;
        9: z <= 16'd78;
        10: z <= -16'd93;
        11: z <= 16'd64;
        12: z <= 16'd13;
        13: z <= -16'd103;
        14: z <= -16'd88;
        15: z <= -16'd125;
        16: z <= 16'd25;
        17: z <= -16'd64;
        18: z <= -16'd102;
        19: z <= 16'd85;
        20: z <= 16'd116;
        21: z <= 16'd25;
        22: z <= -16'd124;
        23: z <= -16'd65;
        24: z <= 16'd64;
        25: z <= -16'd73;
        26: z <= 16'd81;
        27: z <= 16'd10;
        28: z <= 16'd13;
        29: z <= 16'd113;
        30: z <= 16'd76;
        31: z <= 16'd110;
        32: z <= -16'd46;
        33: z <= -16'd76;
        34: z <= -16'd63;
        35: z <= 16'd33;
        36: z <= -16'd31;
        37: z <= 16'd125;
        38: z <= -16'd98;
        39: z <= 16'd40;
        40: z <= 16'd75;
        41: z <= -16'd63;
        42: z <= -16'd24;
        43: z <= -16'd40;
        44: z <= -16'd38;
        45: z <= 16'd16;
        46: z <= -16'd37;
        47: z <= 16'd116;
        48: z <= 16'd80;
        49: z <= -16'd10;
        50: z <= 16'd73;
        51: z <= 16'd69;
        52: z <= -16'd113;
        53: z <= 16'd77;
        54: z <= -16'd124;
        55: z <= 16'd79;
        56: z <= -16'd124;
        57: z <= 16'd85;
        58: z <= -16'd39;
        59: z <= 16'd17;
        60: z <= 16'd71;
        61: z <= -16'd91;
        62: z <= 16'd0;
        63: z <= -16'd103;
        64: z <= -16'd39;
        65: z <= 16'd65;
        66: z <= 16'd59;
        67: z <= 16'd59;
        68: z <= 16'd62;
        69: z <= 16'd89;
        70: z <= -16'd29;
        71: z <= 16'd10;
        72: z <= -16'd102;
        73: z <= 16'd75;
        74: z <= 16'd98;
        75: z <= -16'd12;
        76: z <= -16'd36;
        77: z <= -16'd66;
        78: z <= -16'd24;
        79: z <= -16'd84;
        80: z <= 16'd52;
        81: z <= -16'd79;
        82: z <= 16'd113;
        83: z <= 16'd67;
        84: z <= 16'd127;
        85: z <= 16'd117;
        86: z <= 16'd18;
        87: z <= -16'd125;
        88: z <= 16'd75;
        89: z <= 16'd107;
        90: z <= 16'd21;
        91: z <= 16'd18;
        92: z <= -16'd112;
        93: z <= -16'd107;
        94: z <= 16'd43;
        95: z <= -16'd23;
      endcase
   end
endmodule

module layer2_12_8_1_16_B_rom_0(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd86;
        1: z <= -16'd26;
        2: z <= -16'd92;
        3: z <= 16'd20;
        4: z <= -16'd65;
        5: z <= 16'd7;
        6: z <= -16'd98;
        7: z <= -16'd39;
        8: z <= -16'd45;
        9: z <= -16'd127;
        10: z <= 16'd78;
        11: z <= 16'd47;
      endcase
   end
endmodule


module layer3_16_12_1_16 #(
   parameter M = 16,
   parameter N = 12,
   parameter P = 1,
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

layer3_16_12_1_16_W_rom_0 u_w_rom_0 (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr[0]      ),
       .z       ( rom_w_data_out[0]     ));

layer3_16_12_1_16_B_rom_0 u_b_rom_0 (
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

module layer3_16_12_1_16_W_rom_0(clk, addr, z);
   input clk;
   input [7:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd65;
        1: z <= -16'd74;
        2: z <= 16'd91;
        3: z <= 16'd115;
        4: z <= -16'd24;
        5: z <= 16'd77;
        6: z <= 16'd54;
        7: z <= -16'd25;
        8: z <= 16'd66;
        9: z <= -16'd56;
        10: z <= -16'd22;
        11: z <= 16'd13;
        12: z <= -16'd77;
        13: z <= 16'd127;
        14: z <= -16'd97;
        15: z <= -16'd61;
        16: z <= -16'd108;
        17: z <= 16'd75;
        18: z <= 16'd44;
        19: z <= 16'd106;
        20: z <= -16'd79;
        21: z <= 16'd81;
        22: z <= -16'd1;
        23: z <= -16'd15;
        24: z <= -16'd40;
        25: z <= 16'd29;
        26: z <= 16'd74;
        27: z <= 16'd43;
        28: z <= 16'd30;
        29: z <= 16'd24;
        30: z <= -16'd38;
        31: z <= 16'd93;
        32: z <= 16'd79;
        33: z <= -16'd74;
        34: z <= 16'd80;
        35: z <= -16'd73;
        36: z <= -16'd125;
        37: z <= 16'd6;
        38: z <= 16'd30;
        39: z <= 16'd69;
        40: z <= 16'd78;
        41: z <= -16'd120;
        42: z <= -16'd45;
        43: z <= -16'd127;
        44: z <= -16'd120;
        45: z <= -16'd14;
        46: z <= -16'd60;
        47: z <= -16'd100;
        48: z <= -16'd67;
        49: z <= 16'd113;
        50: z <= -16'd121;
        51: z <= -16'd17;
        52: z <= 16'd66;
        53: z <= 16'd6;
        54: z <= 16'd96;
        55: z <= -16'd102;
        56: z <= -16'd93;
        57: z <= 16'd42;
        58: z <= 16'd70;
        59: z <= 16'd66;
        60: z <= -16'd61;
        61: z <= -16'd96;
        62: z <= 16'd31;
        63: z <= -16'd110;
        64: z <= -16'd42;
        65: z <= -16'd16;
        66: z <= -16'd55;
        67: z <= -16'd39;
        68: z <= 16'd118;
        69: z <= 16'd103;
        70: z <= -16'd97;
        71: z <= 16'd69;
        72: z <= 16'd111;
        73: z <= -16'd14;
        74: z <= 16'd70;
        75: z <= 16'd119;
        76: z <= 16'd100;
        77: z <= -16'd117;
        78: z <= -16'd108;
        79: z <= -16'd94;
        80: z <= 16'd124;
        81: z <= -16'd101;
        82: z <= 16'd17;
        83: z <= 16'd62;
        84: z <= 16'd33;
        85: z <= -16'd15;
        86: z <= 16'd88;
        87: z <= 16'd68;
        88: z <= -16'd101;
        89: z <= 16'd30;
        90: z <= 16'd6;
        91: z <= -16'd34;
        92: z <= 16'd63;
        93: z <= -16'd90;
        94: z <= -16'd16;
        95: z <= -16'd107;
        96: z <= 16'd22;
        97: z <= 16'd57;
        98: z <= -16'd17;
        99: z <= 16'd12;
        100: z <= 16'd32;
        101: z <= 16'd14;
        102: z <= -16'd47;
        103: z <= 16'd16;
        104: z <= -16'd128;
        105: z <= -16'd104;
        106: z <= 16'd7;
        107: z <= 16'd100;
        108: z <= -16'd93;
        109: z <= 16'd27;
        110: z <= -16'd122;
        111: z <= -16'd97;
        112: z <= 16'd54;
        113: z <= 16'd23;
        114: z <= 16'd93;
        115: z <= -16'd41;
        116: z <= -16'd120;
        117: z <= 16'd53;
        118: z <= -16'd100;
        119: z <= -16'd92;
        120: z <= -16'd44;
        121: z <= 16'd34;
        122: z <= 16'd2;
        123: z <= -16'd109;
        124: z <= 16'd72;
        125: z <= 16'd115;
        126: z <= -16'd88;
        127: z <= -16'd34;
        128: z <= 16'd44;
        129: z <= 16'd23;
        130: z <= 16'd107;
        131: z <= -16'd51;
        132: z <= -16'd91;
        133: z <= -16'd68;
        134: z <= 16'd93;
        135: z <= -16'd91;
        136: z <= -16'd44;
        137: z <= -16'd28;
        138: z <= -16'd118;
        139: z <= -16'd9;
        140: z <= -16'd128;
        141: z <= -16'd112;
        142: z <= 16'd22;
        143: z <= 16'd54;
        144: z <= 16'd40;
        145: z <= -16'd13;
        146: z <= -16'd114;
        147: z <= 16'd48;
        148: z <= -16'd87;
        149: z <= -16'd86;
        150: z <= 16'd84;
        151: z <= -16'd3;
        152: z <= 16'd76;
        153: z <= -16'd41;
        154: z <= 16'd16;
        155: z <= 16'd21;
        156: z <= -16'd54;
        157: z <= 16'd56;
        158: z <= 16'd115;
        159: z <= 16'd118;
        160: z <= -16'd48;
        161: z <= 16'd94;
        162: z <= -16'd61;
        163: z <= -16'd11;
        164: z <= -16'd101;
        165: z <= -16'd96;
        166: z <= 16'd27;
        167: z <= -16'd17;
        168: z <= 16'd5;
        169: z <= 16'd37;
        170: z <= 16'd103;
        171: z <= 16'd5;
        172: z <= 16'd53;
        173: z <= -16'd3;
        174: z <= -16'd69;
        175: z <= -16'd35;
        176: z <= 16'd113;
        177: z <= -16'd55;
        178: z <= -16'd114;
        179: z <= -16'd102;
        180: z <= -16'd13;
        181: z <= 16'd98;
        182: z <= 16'd23;
        183: z <= -16'd64;
        184: z <= -16'd71;
        185: z <= -16'd89;
        186: z <= 16'd85;
        187: z <= 16'd3;
        188: z <= 16'd95;
        189: z <= 16'd72;
        190: z <= -16'd6;
        191: z <= -16'd81;
      endcase
   end
endmodule

module layer3_16_12_1_16_B_rom_0(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd39;
        1: z <= 16'd61;
        2: z <= 16'd37;
        3: z <= 16'd66;
        4: z <= 16'd94;
        5: z <= -16'd64;
        6: z <= -16'd79;
        7: z <= -16'd29;
        8: z <= 16'd101;
        9: z <= -16'd104;
        10: z <= 16'd104;
        11: z <= 16'd26;
        12: z <= 16'd22;
        13: z <= -16'd93;
        14: z <= 16'd120;
        15: z <= 16'd7;
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
