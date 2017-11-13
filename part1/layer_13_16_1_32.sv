// ------------------------------------------//
// Neural Network Layer Generator - Part 1     
// ------------------------------------------//
// NAME:  Atif Iqbal                           
// NETID: aahangar                             
// SBUID: 111416569                            
// ------------------------------------------//


module layer_13_16_1_32 #(
   parameter M = 13,
   parameter N = 16,
   parameter P = 1,
   parameter T = 32)
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

   layer_13_16_1_32_W_rom u_w_rom (
       .clk     ( clk                   ),
       .addr    ( rom_w_rd_addr         ),
       .z       ( rom_w_data_out        ));

   layer_13_16_1_32_B_rom u_b_rom (
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

module layer_13_16_1_32_W_rom(clk, addr, z);
   input clk;
   input [7:0] addr;
   output logic signed [31:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -32'd27189;
        1: z <= -32'd433;
        2: z <= 32'd20347;
        3: z <= 32'd16419;
        4: z <= 32'd2769;
        5: z <= -32'd31466;
        6: z <= 32'd11834;
        7: z <= 32'd414;
        8: z <= -32'd26113;
        9: z <= -32'd31298;
        10: z <= 32'd2893;
        11: z <= 32'd22121;
        12: z <= -32'd8238;
        13: z <= 32'd9475;
        14: z <= -32'd15075;
        15: z <= 32'd13768;
        16: z <= -32'd15436;
        17: z <= 32'd15675;
        18: z <= 32'd27447;
        19: z <= -32'd21456;
        20: z <= 32'd15460;
        21: z <= -32'd19743;
        22: z <= 32'd21943;
        23: z <= 32'd30322;
        24: z <= 32'd1897;
        25: z <= 32'd2740;
        26: z <= 32'd30551;
        27: z <= 32'd6567;
        28: z <= -32'd18982;
        29: z <= -32'd30750;
        30: z <= 32'd29327;
        31: z <= -32'd13402;
        32: z <= 32'd1585;
        33: z <= 32'd16906;
        34: z <= -32'd29751;
        35: z <= -32'd28414;
        36: z <= 32'd18208;
        37: z <= 32'd14851;
        38: z <= 32'd4768;
        39: z <= 32'd24864;
        40: z <= 32'd16322;
        41: z <= -32'd25107;
        42: z <= 32'd14217;
        43: z <= -32'd24684;
        44: z <= 32'd17136;
        45: z <= 32'd31911;
        46: z <= 32'd21852;
        47: z <= -32'd31067;
        48: z <= 32'd14818;
        49: z <= 32'd16532;
        50: z <= -32'd19755;
        51: z <= -32'd2490;
        52: z <= 32'd29557;
        53: z <= -32'd30580;
        54: z <= -32'd4936;
        55: z <= -32'd1313;
        56: z <= 32'd4929;
        57: z <= -32'd7152;
        58: z <= -32'd27514;
        59: z <= 32'd18715;
        60: z <= -32'd5134;
        61: z <= -32'd30955;
        62: z <= -32'd27455;
        63: z <= 32'd29219;
        64: z <= 32'd18720;
        65: z <= -32'd24437;
        66: z <= -32'd31963;
        67: z <= 32'd4160;
        68: z <= 32'd23182;
        69: z <= 32'd5573;
        70: z <= -32'd3744;
        71: z <= 32'd6736;
        72: z <= 32'd13234;
        73: z <= -32'd22294;
        74: z <= 32'd14821;
        75: z <= -32'd2397;
        76: z <= -32'd23151;
        77: z <= 32'd3905;
        78: z <= -32'd696;
        79: z <= 32'd24435;
        80: z <= -32'd12331;
        81: z <= 32'd12317;
        82: z <= -32'd10823;
        83: z <= -32'd15541;
        84: z <= 32'd14506;
        85: z <= 32'd17009;
        86: z <= 32'd15914;
        87: z <= -32'd13333;
        88: z <= -32'd22911;
        89: z <= 32'd21168;
        90: z <= -32'd27386;
        91: z <= 32'd4723;
        92: z <= 32'd22981;
        93: z <= -32'd22072;
        94: z <= 32'd1174;
        95: z <= 32'd8933;
        96: z <= -32'd13741;
        97: z <= 32'd1979;
        98: z <= -32'd19674;
        99: z <= -32'd23327;
        100: z <= -32'd25216;
        101: z <= 32'd9350;
        102: z <= 32'd16178;
        103: z <= 32'd20787;
        104: z <= 32'd19824;
        105: z <= -32'd1769;
        106: z <= -32'd14378;
        107: z <= 32'd29441;
        108: z <= -32'd30632;
        109: z <= 32'd17694;
        110: z <= 32'd21108;
        111: z <= -32'd10194;
        112: z <= -32'd2757;
        113: z <= -32'd22483;
        114: z <= 32'd7033;
        115: z <= -32'd21019;
        116: z <= 32'd27295;
        117: z <= -32'd9821;
        118: z <= -32'd1584;
        119: z <= -32'd28384;
        120: z <= -32'd21421;
        121: z <= 32'd3799;
        122: z <= 32'd9108;
        123: z <= -32'd31208;
        124: z <= 32'd14495;
        125: z <= -32'd22486;
        126: z <= 32'd10494;
        127: z <= -32'd32014;
        128: z <= 32'd12262;
        129: z <= 32'd23588;
        130: z <= -32'd22573;
        131: z <= 32'd19814;
        132: z <= 32'd170;
        133: z <= 32'd26373;
        134: z <= 32'd7833;
        135: z <= -32'd12773;
        136: z <= -32'd8164;
        137: z <= 32'd26223;
        138: z <= -32'd16100;
        139: z <= -32'd6027;
        140: z <= 32'd11149;
        141: z <= -32'd27759;
        142: z <= 32'd16547;
        143: z <= -32'd24375;
        144: z <= -32'd17474;
        145: z <= -32'd9188;
        146: z <= -32'd12626;
        147: z <= -32'd22947;
        148: z <= 32'd13759;
        149: z <= 32'd18559;
        150: z <= -32'd18562;
        151: z <= 32'd25106;
        152: z <= -32'd10410;
        153: z <= 32'd23314;
        154: z <= 32'd26666;
        155: z <= -32'd28683;
        156: z <= -32'd31940;
        157: z <= 32'd4392;
        158: z <= -32'd27929;
        159: z <= 32'd13090;
        160: z <= -32'd4788;
        161: z <= -32'd17734;
        162: z <= 32'd137;
        163: z <= 32'd28151;
        164: z <= -32'd24128;
        165: z <= -32'd24798;
        166: z <= -32'd17390;
        167: z <= 32'd476;
        168: z <= -32'd31342;
        169: z <= -32'd722;
        170: z <= 32'd27217;
        171: z <= 32'd12575;
        172: z <= 32'd4287;
        173: z <= 32'd10996;
        174: z <= 32'd20968;
        175: z <= 32'd19582;
        176: z <= -32'd30960;
        177: z <= -32'd24425;
        178: z <= 32'd29403;
        179: z <= 32'd15567;
        180: z <= 32'd26902;
        181: z <= -32'd21927;
        182: z <= 32'd7905;
        183: z <= -32'd16276;
        184: z <= -32'd31381;
        185: z <= 32'd1804;
        186: z <= -32'd12191;
        187: z <= -32'd30552;
        188: z <= -32'd26572;
        189: z <= -32'd7352;
        190: z <= 32'd15306;
        191: z <= 32'd1409;
        192: z <= 32'd7682;
        193: z <= -32'd17325;
        194: z <= -32'd3208;
        195: z <= 32'd16322;
        196: z <= -32'd9354;
        197: z <= 32'd12170;
        198: z <= -32'd15969;
        199: z <= -32'd7928;
        200: z <= -32'd21320;
        201: z <= -32'd21520;
        202: z <= -32'd28121;
        203: z <= 32'd15736;
        204: z <= 32'd22245;
        205: z <= 32'd25616;
        206: z <= 32'd2550;
        207: z <= 32'd24053;
      endcase
   end
endmodule

module layer_13_16_1_32_B_rom(clk, addr, z);
   input clk;
   input [3:0] addr;
   output logic signed [31:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -32'd31577;
        1: z <= -32'd815;
        2: z <= 32'd6853;
        3: z <= 32'd28093;
        4: z <= 32'd10027;
        5: z <= -32'd18010;
        6: z <= -32'd20951;
        7: z <= 32'd11414;
        8: z <= 32'd16562;
        9: z <= -32'd374;
        10: z <= 32'd13630;
        11: z <= 32'd22759;
        12: z <= 32'd25042;
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
