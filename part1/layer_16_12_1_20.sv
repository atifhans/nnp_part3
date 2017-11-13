// ------------------------------------------//
// Neural Network Layer Generator - Part 1     
// ------------------------------------------//
// NAME:  Atif Iqbal                           
// NETID: aahangar                             
// SBUID: 111416569                            
// ------------------------------------------//


module layer_16_12_1_20 #(
   parameter M = 16,
   parameter N = 12,
   parameter P = 1,
   parameter T = 20)
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

   layer_16_12_1_20_W_rom u_w_rom (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr         ),
       .z       ( rom_w_data_out        ));

   layer_16_12_1_20_B_rom u_b_rom (
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

module layer_16_12_1_20_W_rom(clk, addr, z);
   input clk;
   input [7:0] addr;
   output logic signed [19:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 20'd197;
        1: z <= 20'd340;
        2: z <= -20'd31;
        3: z <= -20'd202;
        4: z <= 20'd507;
        5: z <= 20'd442;
        6: z <= -20'd235;
        7: z <= -20'd442;
        8: z <= 20'd278;
        9: z <= -20'd392;
        10: z <= 20'd383;
        11: z <= 20'd103;
        12: z <= 20'd85;
        13: z <= 20'd325;
        14: z <= -20'd81;
        15: z <= -20'd311;
        16: z <= -20'd161;
        17: z <= -20'd195;
        18: z <= -20'd226;
        19: z <= -20'd511;
        20: z <= 20'd308;
        21: z <= -20'd496;
        22: z <= -20'd428;
        23: z <= -20'd251;
        24: z <= -20'd47;
        25: z <= 20'd468;
        26: z <= -20'd40;
        27: z <= -20'd222;
        28: z <= 20'd332;
        29: z <= -20'd242;
        30: z <= -20'd377;
        31: z <= 20'd17;
        32: z <= -20'd414;
        33: z <= 20'd104;
        34: z <= 20'd327;
        35: z <= -20'd419;
        36: z <= 20'd34;
        37: z <= -20'd420;
        38: z <= -20'd348;
        39: z <= -20'd200;
        40: z <= -20'd299;
        41: z <= -20'd477;
        42: z <= 20'd416;
        43: z <= 20'd298;
        44: z <= 20'd361;
        45: z <= -20'd177;
        46: z <= 20'd499;
        47: z <= -20'd312;
        48: z <= 20'd140;
        49: z <= -20'd239;
        50: z <= -20'd310;
        51: z <= -20'd64;
        52: z <= -20'd222;
        53: z <= -20'd226;
        54: z <= 20'd197;
        55: z <= 20'd243;
        56: z <= -20'd270;
        57: z <= -20'd355;
        58: z <= -20'd490;
        59: z <= -20'd450;
        60: z <= -20'd84;
        61: z <= -20'd355;
        62: z <= 20'd79;
        63: z <= 20'd14;
        64: z <= 20'd261;
        65: z <= -20'd106;
        66: z <= 20'd108;
        67: z <= -20'd217;
        68: z <= -20'd13;
        69: z <= 20'd272;
        70: z <= 20'd95;
        71: z <= 20'd200;
        72: z <= 20'd307;
        73: z <= -20'd1;
        74: z <= -20'd14;
        75: z <= 20'd156;
        76: z <= 20'd334;
        77: z <= -20'd26;
        78: z <= 20'd357;
        79: z <= -20'd38;
        80: z <= 20'd247;
        81: z <= -20'd465;
        82: z <= 20'd411;
        83: z <= -20'd487;
        84: z <= -20'd179;
        85: z <= 20'd96;
        86: z <= 20'd269;
        87: z <= 20'd63;
        88: z <= 20'd254;
        89: z <= 20'd291;
        90: z <= 20'd126;
        91: z <= -20'd342;
        92: z <= 20'd448;
        93: z <= -20'd307;
        94: z <= 20'd184;
        95: z <= 20'd197;
        96: z <= 20'd100;
        97: z <= -20'd220;
        98: z <= 20'd492;
        99: z <= -20'd425;
        100: z <= -20'd460;
        101: z <= 20'd75;
        102: z <= 20'd287;
        103: z <= 20'd360;
        104: z <= -20'd437;
        105: z <= -20'd239;
        106: z <= 20'd4;
        107: z <= 20'd409;
        108: z <= 20'd247;
        109: z <= -20'd151;
        110: z <= -20'd140;
        111: z <= -20'd17;
        112: z <= -20'd104;
        113: z <= -20'd241;
        114: z <= 20'd8;
        115: z <= 20'd229;
        116: z <= 20'd367;
        117: z <= -20'd235;
        118: z <= -20'd219;
        119: z <= 20'd109;
        120: z <= -20'd456;
        121: z <= 20'd419;
        122: z <= 20'd279;
        123: z <= 20'd504;
        124: z <= -20'd400;
        125: z <= -20'd48;
        126: z <= 20'd189;
        127: z <= 20'd212;
        128: z <= 20'd244;
        129: z <= 20'd169;
        130: z <= 20'd299;
        131: z <= 20'd297;
        132: z <= -20'd267;
        133: z <= 20'd74;
        134: z <= 20'd145;
        135: z <= -20'd192;
        136: z <= 20'd348;
        137: z <= -20'd363;
        138: z <= -20'd295;
        139: z <= 20'd83;
        140: z <= -20'd1;
        141: z <= 20'd77;
        142: z <= -20'd446;
        143: z <= 20'd407;
        144: z <= 20'd348;
        145: z <= 20'd75;
        146: z <= 20'd125;
        147: z <= 20'd204;
        148: z <= 20'd352;
        149: z <= 20'd418;
        150: z <= -20'd199;
        151: z <= 20'd409;
        152: z <= 20'd325;
        153: z <= -20'd431;
        154: z <= 20'd401;
        155: z <= 20'd437;
        156: z <= 20'd33;
        157: z <= 20'd79;
        158: z <= 20'd138;
        159: z <= -20'd235;
        160: z <= -20'd264;
        161: z <= -20'd75;
        162: z <= -20'd450;
        163: z <= -20'd19;
        164: z <= -20'd512;
        165: z <= 20'd207;
        166: z <= 20'd301;
        167: z <= 20'd348;
        168: z <= 20'd357;
        169: z <= -20'd505;
        170: z <= -20'd81;
        171: z <= -20'd156;
        172: z <= 20'd84;
        173: z <= -20'd14;
        174: z <= -20'd261;
        175: z <= -20'd79;
        176: z <= -20'd451;
        177: z <= 20'd376;
        178: z <= -20'd387;
        179: z <= 20'd413;
        180: z <= 20'd282;
        181: z <= -20'd74;
        182: z <= 20'd310;
        183: z <= 20'd95;
        184: z <= 20'd7;
        185: z <= 20'd200;
        186: z <= 20'd21;
        187: z <= -20'd472;
        188: z <= -20'd233;
        189: z <= -20'd353;
        190: z <= -20'd194;
        191: z <= 20'd15;
      endcase
   end
endmodule

module layer_16_12_1_20_B_rom(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [19:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 20'd84;
        1: z <= -20'd132;
        2: z <= 20'd509;
        3: z <= 20'd84;
        4: z <= -20'd436;
        5: z <= 20'd298;
        6: z <= -20'd80;
        7: z <= 20'd433;
        8: z <= 20'd305;
        9: z <= 20'd352;
        10: z <= -20'd235;
        11: z <= -20'd122;
        12: z <= -20'd174;
        13: z <= 20'd16;
        14: z <= 20'd311;
        15: z <= -20'd113;
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
