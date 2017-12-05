// ESE 507 Project 3 Handout Code
// Fall 2017
// Peter Milder

// Getting started:
// The main() function contains the code to read the parameters.
// For Parts 1 and 2, your code should be in the genLayer() function. Please
// also look at this function to see an example for how to create the ROMs.
//
// For Part 3, your code should be in the genAllLayers() function.


#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cstdlib>
#include <cstring>
#include <assert.h>
#include <math.h>
using namespace std;

void printUsage();
void genLayer(int M, int N, int P, int bits, vector<int>& constvector, string modName, ofstream &os);
void genAllLayers(int N, int M1, int M2, int M3, int mult_budget, int bits, vector<int>& constVector, string modName, ofstream &os);
void readConstants(ifstream &constStream, vector<int>& constvector);
void genROM(vector<int>& constVector, int bits, string modName, ofstream &os);


int main(int argc, char* argv[]) {

   // If the user runs the program without enough parameters, print a helpful message
   // and quit.
   if (argc < 7) {
      printUsage();
      return 1;
   }

   int mode = atoi(argv[1]);

   ifstream const_file;
   ofstream os;
   vector<int> constVector;

   //----------------------------------------------------------------------
   // Look here for Part 1 and 2
   if ((mode == 1) && (argc == 7)) {
      // Mode 1: Generate one layer with given dimensions and one testbench

      // --------------- read parameters, etc. ---------------
      int M = atoi(argv[2]);
      int N = atoi(argv[3]);
      int P = atoi(argv[4]);
      int bits = atoi(argv[5]);
      const_file.open(argv[6]);
      if (const_file.is_open() != true) {
         cout << "ERROR reading constant file " << argv[6] << endl;
         return 1;
      }

      // Read the constants out of the provided file and place them in the constVector vector
      readConstants(const_file, constVector);

      string out_file = "layer_" + to_string(M) + "_" + to_string(N) + "_" + to_string(P) + "_" + to_string(bits) + ".sv";

      os.open(out_file);
      if (os.is_open() != true) {
         cout << "ERROR opening " << out_file << " for write." << endl;
         return 1;
      }
      // -------------------------------------------------------------

      // call the genLayer function you will write to generate this layer
      string modName = "layer_" + to_string(M) + "_" + to_string(N) + "_" + to_string(P) + "_" + to_string(bits);
      genLayer(M, N, P, bits, constVector, modName, os);

   }
   //--------------------------------------------------------------------


   // ----------------------------------------------------------------
   // Look here for Part 3
   else if ((mode == 2) && (argc == 9)) {
      // Mode 2: Generate three layer with given dimensions and interconnect them

      // --------------- read parameters, etc. ---------------
      int N  = atoi(argv[2]);
      int M1 = atoi(argv[3]);
      int M2 = atoi(argv[4]);
      int M3 = atoi(argv[5]);
      int mult_budget = atoi(argv[6]);
      int bits = atoi(argv[7]);
      const_file.open(argv[8]);
      if (const_file.is_open() != true) {
         cout << "ERROR reading constant file " << argv[8] << endl;
         return 1;
      }
      readConstants(const_file, constVector);

      string out_file = "network_" + to_string(N) + "_" + to_string(M1) + "_" + to_string(M2) + "_" + to_string(M3) + "_" + to_string(mult_budget) + "_" + to_string(bits) + ".sv";


      os.open(out_file);
      if (os.is_open() != true) {
         cout << "ERROR opening " << out_file << " for write." << endl;
         return 1;
      }
      // -------------------------------------------------------------

      string mod_name = "network_" + to_string(N) + "_" + to_string(M1) + "_" + to_string(M2) + "_" + to_string(M3) + "_" + to_string(mult_budget) + "_" + to_string(bits);

      // call the genAllLayers function
      genAllLayers(N, M1, M2, M3, mult_budget, bits, constVector, mod_name, os);

   }
   //-------------------------------------------------------

   else {
      printUsage();
      return 1;
   }

   // close the output stream
   os.close();

   //int M = atoi(argv[2]);
   //int N = atoi(argv[3]);
   //int P = atoi(argv[4]);
   //int bits = atoi(argv[5]);

   //string out_file = "layer_" + to_string(M) + "_" + to_string(N) + "_" + to_string(P) + "_" + to_string(bits) + ".sv";

   //string append_mac_cmd = "cat mac.sv >> " + out_file;
   //string append_mem_cmd = "cat memory.sv >> " + out_file;

   //ystem(append_mac_cmd.c_str());
   //system(append_mem_cmd.c_str());

}

// Read values from the constant file into the vector
void readConstants(ifstream &constStream, vector<int>& constvector) {
   string constLineString;
   while(getline(constStream, constLineString)) {
      int val = atoi(constLineString.c_str());
      constvector.push_back(val);
   }
}

// Generate a ROM based on values constVector.
// Values should each be "bits" number of bits.
void genROM(vector<int>& constVector, int bits, string modName, ofstream &os) {

      int numWords = constVector.size();
      int addrBits = ceil(log2(numWords));

      os << "module " << modName << "(clk, addr, z);" << endl;
      os << "   input clk;" << endl;
      os << "   input [" << addrBits-1 << ":0] addr;" << endl;
      os << "   output logic signed [" << bits-1 << ":0] z;" << endl;
      os << "   always_ff @(posedge clk) begin" << endl;
      os << "      case(addr)" << endl;
      int i=0;
      for (vector<int>::iterator it = constVector.begin(); it < constVector.end(); it++, i++) {
         if (*it < 0)
            os << "        " << i << ": z <= -" << bits << "'d" << abs(*it) << ";" << endl;
         else
            os << "        " << i << ": z <= "  << bits << "'d" << *it      << ";" << endl;
      }
      os << "      endcase" << endl << "   end" << endl << "endmodule" << endl << endl;
}

// Parts 1 and 2
// Here is where you add your code to produce a neural network layer.
void genLayer(int M, int N, int P, int bits, vector<int>& constVector, string modName, ofstream &os) {

   // Make your module name: layer_M_N_P_bits, where these parameters are replaced with the
   // actual numbers

   //Generating File Header
   os << "// ------------------------------------------//" << endl;
   os << "// Neural Network Layer Generator - Part 1     " << endl;
   os << "// ------------------------------------------//" << endl;
   os << "// NAME:  Atif Iqbal                           " << endl;
   os << "// NETID: aahangar                             " << endl;
   os << "// SBUID: 111416569                            " << endl;
   os << "// ------------------------------------------//" << endl;
   os << endl << endl;

   //Generating Module Header
   os << "module " << modName << " #(" << endl;
   os << "   parameter M = " << M << "," << endl;  //TOOD: implicit conversion. Use to_string
   os << "   parameter N = " << N << "," << endl;
   os << "   parameter P = " << P << "," << endl;
   os << "   parameter T = " << bits << ")" << endl;
   os << "(" << endl;
   os << "   input  logic                   clk," << endl;
   os << "   input  logic                   reset," << endl;
   os << "   input  logic                   s_valid," << endl;
   os << "   input  logic                   m_ready," << endl;
   os << "   input  logic signed [T-1:0]    data_in," << endl;
   os << "   output logic                   m_valid," << endl;
   os << "   output logic                   s_ready," << endl;
   os << "   output logic signed [T-1:0]    data_out" << endl;
   os << ");" << endl << endl;

   //Generating Parameters.
   os << "   localparam MAT_W_SIZE = M * N;" << endl;
   os << "   localparam VEC_b_SIZE = M;" << endl;
   os << "   localparam VEC_x_SIZE = N;" << endl;
   os << "   localparam VEC_y_SIZE = M;" << endl;
   os << "   localparam MAT_W_ADDW = $clog2(MAT_W_SIZE);" << endl;
   os << "   localparam VEC_b_ADDW = $clog2(VEC_b_SIZE);" << endl;
   os << "   localparam VEC_x_ADDW = $clog2(VEC_x_SIZE+1);" << endl;
   os << "   localparam VEC_y_ADDW = $clog2(VEC_y_SIZE);" << endl;
   os << endl;

   //Generating Internal Variables.
   os << "   enum logic [1:0] {GET_x=0, COMPUTE_y=1} state, next_state;" << endl;
   os << "   logic        [MAT_W_ADDW-1:0] rom_w_rd_addr;" << endl;
   os << "   logic        [VEC_b_ADDW-1:0] rom_b_rd_addr;" << endl;
   os << "   logic        [VEC_x_ADDW-1:0] ram_x_addr;" << endl;
   os << "   logic        [VEC_x_ADDW-1:0] ram_x_wr_addr;" << endl;
   os << "   logic        [VEC_x_ADDW-1:0] ram_x_rd_addr;" << endl;
   os << "   logic signed          [T-1:0] rom_w_data_out;" << endl;
   os << "   logic                 [T-1:0] rom_b_data_out;" << endl;
   os << "   logic signed          [T-1:0] ram_x_data_out;" << endl;
   os << "   logic signed          [T-1:0] mac_data_out;" << endl;
   os << "   logic                         ram_x_wr_en;" << endl;
   os << "   logic        [VEC_x_ADDW-1:0] vec_cnt;" << endl;
   os << "   logic                         next_req;" << endl;
   os << "   logic                         mac_valid_in;" << endl;
   os << "   logic                         mac_valid_out;" << endl;
   os << "   logic                         compute_done;" << endl;
   os << endl;

   //Generating Assign Statements
   os << "   assign s_ready = (state == GET_x);" << endl;
   os << "   assign ram_x_wr_en = s_ready & s_valid;" << endl;
   os << "   assign ram_x_addr = (state == GET_x) ? ram_x_wr_addr : ram_x_rd_addr;" << endl;
   os << "   assign data_out = (mac_data_out < $signed(0)) ? 0 : mac_data_out;" << endl;
   //os << "   assign mac_valid_in = next_req && (state == COMPUTE_y);" << endl;
   os << endl;

   //Generating instantiations.
   //Vec x memory
   os << "   memory #(" << endl;
   os << "      .WIDTH    ( T                     )," << endl;
   os << "      .SIZE     ( VEC_x_SIZE            )," << endl;
   os << "      .LOGSIZE  ( VEC_x_ADDW            ))" << endl;
   os << "   u_vec_x_mem (" << endl;
   os << "      .clk      ( clk                   )," << endl;
   os << "      .data_in  ( data_in               )," << endl;
   os << "      .data_out ( ram_x_data_out        )," << endl;
   os << "      .addr     ( ram_x_addr            )," << endl;
   os << "      .wr_en    ( ram_x_wr_en           ));" << endl;
   os << endl;

   //MAT a rom
   os << "   layer_" << M << "_" << N << "_" << P << "_" << bits << "_W_rom u_w_rom (" << endl;
   os << "       .clk     ( clk                   )," << endl;
   os << "       .addr    ( rom_w_rd_addr         )," << endl;
   os << "       .z       ( rom_w_data_out        ));" << endl;
   os << endl;

   //Vec b rom
   os << "   layer_" << M << "_" << N << "_" << P << "_" << bits << "_B_rom u_b_rom (" << endl;
   os << "       .clk     ( clk                   )," << endl;
   os << "       .addr    ( rom_b_rd_addr         )," << endl;
   os << "       .z       ( rom_b_data_out        ));" << endl;
   os << endl;

   //MAC
   os << "   part3_mac #(" << endl;
   os << "      .T         ( T                    )," << endl;
   os << "      .NUM_S     ( 1                    )," << endl; //TODO: change this to param.
   os << "      .VEC_S     ( VEC_x_SIZE           ))" << endl;
   os << "   u_mac (" << endl;
   os << "      .clk       ( clk                  )," << endl;
   os << "      .reset     ( reset                )," << endl;
   os << "      .a         ( rom_w_data_out       )," << endl;
   os << "      .b         ( rom_b_data_out       )," << endl;
   os << "      .x         ( ram_x_data_out       )," << endl;
   os << "      .valid_in  ( mac_valid_in         )," << endl;
   os << "      .f         ( mac_data_out         )," << endl;
   os << "      .valid_out ( mac_valid_out        )," << endl;
   os << "      .overflow  ( /* Not Used */       ));" << endl;
   os << endl;

   //FSM logic.
   os << "   always_ff @(posedge clk)" << endl;
   os << "      if(reset) begin" << endl;
   os << "          state <= GET_x;" << endl;
   os << "      end" << endl;
   os << "      else begin" << endl;
   os << "          state <= next_state;" << endl;
   os << "      end" << endl;
   os << endl;

   os << "   always_comb begin" << endl;
   os << "      next_state = GET_x;" << endl;
   os << "      case (state)" << endl;
   os << "          GET_x: begin" << endl;
   os << "             if(ram_x_wr_addr == VEC_x_SIZE-1 && s_valid)" << endl;
   os << "                next_state = COMPUTE_y;" << endl;
   os << "             else" << endl;
   os << "                next_state = GET_x;" << endl;
   os << "          end" << endl;
   os << "          COMPUTE_y: begin" << endl;
   os << "             if(compute_done && m_valid && m_ready)" << endl;
   os << "                next_state = GET_x;" << endl;
   os << "             else" << endl;
   os << "                next_state = COMPUTE_y;" << endl;
   os << "          end" << endl;
   os << "      endcase" << endl;
   os << "   end" << endl;
   os << endl;

   //Other sequential logic.
   os << "   always_ff @(posedge clk)" << endl;
   os << "      if(reset) begin" << endl;
   os << "         ram_x_wr_addr <= 'd0;" << endl;
   os << "      end" << endl;
   os << "      else begin" << endl;
   os << "         if(ram_x_wr_addr == VEC_x_SIZE-1 && s_valid) begin" << endl;
   os << "             ram_x_wr_addr <= 'd0;" << endl;
   os << "         end" << endl;
   os << "         else if (state == GET_x && s_valid) begin" << endl;
   os << "             ram_x_wr_addr <= ram_x_wr_addr + 1'd1;" << endl;
   os << "         end" << endl;
   os << "      end" << endl;
   os << endl;

   os << "   always_ff @(posedge clk)" << endl;
   os << "      if(reset) begin" << endl;
   os << "         rom_w_rd_addr <= 'd0;" << endl;
   os << "         rom_b_rd_addr <= 'd0;" << endl;
   os << "         ram_x_rd_addr <= 'd0;" << endl;
   os << "         compute_done  <= 'd0;" << endl;
   os << "      end" << endl;
   os << "      else begin" << endl;
   os << "         if(rom_w_rd_addr == MAT_W_SIZE-1 && next_req) begin" << endl;
   os << "            rom_w_rd_addr <= 'd0;" << endl;
   os << "            rom_b_rd_addr <= 'd0;" << endl;
   os << "            ram_x_rd_addr <= 'd0;" << endl;
   os << "            compute_done  <= 'd1;" << endl;
   os << "         end" << endl;
   os << "         else if (ram_x_rd_addr == VEC_x_SIZE-1 && next_req) begin" << endl;
   os << "            rom_w_rd_addr <= rom_w_rd_addr + 1'd1;" << endl;
   os << "            rom_b_rd_addr <= rom_b_rd_addr + 1'd1;" << endl;
   os << "            ram_x_rd_addr <= 'd0;" << endl;
   os << "            compute_done  <= 'd0;" << endl;
   os << "         end" << endl;
   os << "         else if ((state == COMPUTE_y) && vec_cnt < VEC_x_SIZE && next_req) begin" << endl;
   os << "            rom_w_rd_addr <= rom_w_rd_addr + 1'd1;" << endl;
   os << "            ram_x_rd_addr <= ram_x_rd_addr + 1'd1;" << endl;
   os << "            compute_done  <= 'd0;" << endl;
   os << "         end" << endl;
   os << "      end" << endl;
   os << endl;

   os << "   always_ff @(posedge clk)" << endl;
   os << "      if(reset) begin" << endl;
   os << "         next_req     <= 1'b1;" << endl;
   os << "         mac_valid_in <= 1'b0;" << endl;
   os << "         vec_cnt      <=  'd0;" << endl;
   os << "      end" << endl;
   os << "      else begin" << endl;
   os << "         if(vec_cnt == VEC_x_SIZE) begin" << endl;
   os << "            next_req      <= 1'b0;" << endl;
   os << "            mac_valid_in  <= 1'b0;" << endl;
   os << "            vec_cnt       <= 2'd0;" << endl;
   os << "         end" << endl;
   os << "         else if (m_valid && m_ready) begin" << endl;
   os << "            next_req      <= 1'b1;" << endl;
   os << "         end" << endl;
   os << "         else if (next_req && (state == COMPUTE_y)) begin" << endl;
   os << "            next_req      <= 1'b1;" << endl;
   os << "            mac_valid_in  <= 1'b1;" << endl;
   os << "            vec_cnt       <= vec_cnt + 1'd1;" << endl;
   os << "         end" << endl;
   os << "      end" << endl;
   os << endl;

   os << "   always_ff @(posedge clk)" << endl;
   os << "      if(reset) begin" << endl;
   os << "         m_valid <= 1'b0;" << endl;
   os << "      end" << endl;
   os << "      else begin" << endl;
   os << "         if(mac_valid_out) begin" << endl;
   os << "            m_valid <= 1'b1;" << endl;
   os << "         end" << endl;
   os << "         else if(m_valid && m_ready) begin" << endl;
   os << "            m_valid <= 1'b0;" << endl;
   os << "         end" << endl;
   os << "      end" << endl;
   os << endl;

   os << "endmodule" << endl << endl;

   // At some point you will want to generate a ROM with values from the pre-stored constant values.
   // Here is code that demonstrates how to do this for the simple case where you want to put all of
   // the matrix values W in one ROM, and all of the bias values B into another ROM. (This is probably)
   // what you will need for P=1, but you will want to change this for P>1.


   // Check there are enough values in the constant file.
   if (M*N+M > constVector.size()) {
      cout << "ERROR: constVector does not contain enough data for the requested design" << endl;
      cout << "The design parameters requested require " << M*N+M << " numbers, but the provided data only have " << constVector.size() << " constants" << endl;
      assert(false);
   }

   // Generate a ROM (for W) with constants 0 through M*N-1, with "bits" number of bits
   string romModName = modName + "_W_rom";
   vector<int> wVector(&constVector[0], &constVector[M*N]);
   genROM(wVector, bits, romModName, os);

   // Generate a ROM (for B) with constants M*N through M*N+M-1 wits "bits" number of bits
   romModName = modName + "_B_rom";
   vector<int> bVector(&constVector[M*N], &constVector[M*N+M]);

   genROM(bVector, bits, romModName, os);

   os << "module part3_mac #(parameter T = 8," << endl;
   os << "                   parameter NUM_S =  2," << endl;
   os << "                   parameter VEC_S =  3)" << endl;
   os << "(" << endl;
   os << "    input  logic                clk, " << endl;
   os << "    input  logic                reset," << endl;
   os << "    input  logic signed [T-1:0] a, " << endl;
   os << "    input  logic signed [T-1:0] b,  " << endl;
   os << "    input  logic        [T-1:0] x,  " << endl;
   os << "    input  logic                valid_in," << endl;
   os << "    output logic signed [T-1:0] f, " << endl;
   os << "    output logic                valid_out," << endl;
   os << "    output logic                overflow" << endl;
   os << ");" << endl;
   os << "" << endl;
   os << "    localparam VCNT_LSIZE  = $clog2(VEC_S+1);" << endl;
   os << "" << endl;
   os << "    logic signed [T-1:0]    a_int;" << endl;
   os << "    logic signed [T-1:0]    b_int;" << endl;
   os << "    logic        [T-1:0]    x_int;" << endl;
   os << "    logic signed [T-1:0]    c_int;" << endl;
   os << "    logic signed [T-1:0]    d_int;" << endl;
   os << "    logic signed [T-1:0]    e_int;" << endl;
   os << "    logic                   overflow_int;" << endl;
   os << "    logic                   enable_d;" << endl;
   os << "    logic                   enable_f;" << endl;
   os << "    logic [NUM_S-1:0]       enable_m;" << endl;
   os << "    logic [VCNT_LSIZE-1:0]  vec_cnt;" << endl;
   os << "" << endl;
   os << "    assign e_int = f + d_int;" << endl;
   os << "" << endl;
   os << "    //Simple overflow detection logic" << endl;
   os << "    assign overflow_int = ( f[T-1] &  d_int[T-1] & !e_int[T-1]) |" << endl;
   os << "                          (!f[T-1] & !d_int[T-1] &  e_int[T-1]);" << endl;
   os << "" << endl;
   os << "    generate" << endl;
   os << "        if (NUM_S == 1) begin" << endl;
   os << "            assign c_int = (a_int * x_int);" << endl;
   os << "            assign enable_m[0] = enable_d;" << endl;
   os << "        end" << endl;
   os << "        else begin " << endl;
   os << "            if (NUM_S == 2) begin" << endl;
   os << "                DW02_mult_2_stage #(8, 8) multinstance(a_int, x_int, 1'b1, clk, c_int);" << endl;
   os << "            end" << endl;
   os << "            else if(NUM_S == 3) begin" << endl;
   os << "                DW02_mult_3_stage #(8, 8) multinstance(a_int, x_int, 1'b1, clk, c_int);" << endl;
   os << "            end" << endl;
   os << "            else if(NUM_S == 4) begin" << endl;
   os << "                DW02_mult_4_stage #(8, 8) multinstance(a_int, x_int, 1'b1, clk, c_int);" << endl;
   os << "            end" << endl;
   os << "            else if(NUM_S == 5) begin" << endl;
   os << "                DW02_mult_5_stage #(8, 8) multinstance(a_int, x_int, 1'b1, clk, c_int);" << endl;
   os << "            end" << endl;
   os << "            else if(NUM_S == 6) begin" << endl;
   os << "                DW02_mult_6_stage #(8, 8) multinstance(a_int, x_int, 1'b1, clk, c_int);" << endl;
   os << "            end" << endl;
   os << "" << endl;
   os << "            always_ff @(posedge clk)" << endl;
   os << "                if (reset) begin" << endl;
   os << "                    enable_m <= 'd0;" << endl;
   os << "                end" << endl;
   os << "                else begin" << endl;
   os << "                    enable_m[NUM_S-2] <= enable_d;" << endl;
   os << "                    for(int i = 0; i < NUM_S-2; i++)" << endl;
   os << "                        enable_m[i] <= enable_m[i+1];" << endl;
   os << "                end" << endl;
   os << "        end" << endl;
   os << "    endgenerate" << endl;
   os << "" << endl;
   os << "    //--------------------------------------------------//" << endl;
   os << "    // Flopping the a, b and valid_in input." << endl;
   os << "    //--------------------------------------------------//" << endl;
   os << "    always_ff @(posedge clk)" << endl;
   os << "        if (reset) begin" << endl;
   os << "            a_int    <=  'd0;" << endl;
   os << "            b_int    <=  'd0;" << endl;
   os << "            x_int    <=  'd0;" << endl;
   os << "            enable_d <= 1'b0;" << endl;
   os << "        end" << endl;
   os << "        else if (valid_in) begin" << endl;
   os << "            a_int    <= a;" << endl;
   os << "            b_int    <= b;" << endl;
   os << "            x_int    <= x;" << endl;
   os << "            enable_d <= 1'b1;" << endl;
   os << "        end" << endl;
   os << "        else begin" << endl;
   os << "            enable_d <= 1'b0;" << endl;
   os << "        end" << endl;
   os << "" << endl;
   os << "" << endl;
   os << "    //--------------------------------------------------//" << endl;
   os << "    // Pipeline reg between multiplier and adder." << endl;
   os << "    //--------------------------------------------------//" << endl;
   os << "    always_ff @(posedge clk)" << endl;
   os << "        if (reset) begin" << endl;
   os << "            d_int    <=  'd0;" << endl;
   os << "            enable_f <= 1'b0;" << endl;
   os << "        end" << endl;
   os << "        else if (enable_m[0]) begin" << endl;
   os << "            d_int    <= c_int; " << endl;
   os << "            enable_f <= 1'b1;" << endl;
   os << "        end" << endl;
   os << "        else begin" << endl;
   os << "            enable_f <= 1'b0;" << endl;
   os << "        end" << endl;
   os << "" << endl;
   os << "    //--------------------------------------------------//" << endl;
   os << "    // Doing MAC operation." << endl;
   os << "    //--------------------------------------------------//" << endl;
   os << "    always_ff @(posedge clk)" << endl;
   os << "        if (reset) begin" << endl;
   os << "            f         <=   'd0;" << endl;
   os << "            valid_out <=  1'b0; " << endl;
   os << "            vec_cnt   <=   'd0;" << endl;
   os << "        end" << endl;
   os << "        else if (enable_f) begin" << endl;
   os << "            f         <= (vec_cnt ==    2'd0) ? d_int + b_int : f + d_int;" << endl;
   os << "            vec_cnt   <= (vec_cnt == VEC_S-1) ? 0 : vec_cnt + 1'b1;" << endl;
   os << "            valid_out <= (vec_cnt == VEC_S-1) ? 1'b1 : 1'b0;" << endl;
   os << "        end" << endl;
   os << "        else begin" << endl;
   os << "            valid_out <= 1'b0;" << endl;
   os << "        end" << endl;
   os << "" << endl;
   os << "    //--------------------------------------------------//" << endl;
   os << "    // Overflow detection." << endl;
   os << "    //--------------------------------------------------//" << endl;
   os << "    always_ff @(posedge clk)" << endl;
   os << "        if (reset)" << endl;
   os << "            overflow <= 1'b0; " << endl;
   os << "        else if (vec_cnt == 0)" << endl;
   os << "            overflow <= 1'b0;" << endl;
   os << "        else if (overflow_int & enable_f)" << endl;
   os << "            overflow <= 1'b1;" << endl;
   os << "" << endl;
   os << "endmodule" << endl;

   os << "module memory(clk, data_in, data_out, addr, wr_en);" << endl;
   os << "   " << endl;
   os << "   parameter WIDTH=16, SIZE=64, LOGSIZE=6;" << endl;
   os << "   input [WIDTH-1:0] data_in;" << endl;
   os << "   output logic [WIDTH-1:0] data_out;" << endl;
   os << "   input [LOGSIZE-1:0]      addr;" << endl;
   os << "   input                    clk, wr_en;" << endl;
   os << "   " << endl;
   os << "   logic [SIZE-1:0][WIDTH-1:0] mem;" << endl;
   os << "   " << endl;
   os << "   always_ff @(posedge clk) begin" << endl;
   os << "      data_out <= mem[addr];" << endl;
   os << "	  if (wr_en)" << endl;
   os << "	    mem[addr] <= data_in;" << endl;
   os << "   end" << endl;
   os << "endmodule" << endl;

}

// Part 3: Generate a hardware system with three layers interconnected.
// Layer 1: Input length: N, output length: M1
// Layer 2: Input length: M1, output length: M2
// Layer 3: Input length: M2, output length: M3
// mult_budget is the number of multipliers your overall design may use.
// Your goal is to build the fastest design that uses mult_budget or fewer multipliers
// constVector holds all the constants for your system (all three layers, in order)
void genAllLayers(int N, int M1, int M2, int M3, int mult_budget, int bits, vector<int>& constVector, string modName, ofstream &os) {

   // Here you will write code to figure out the best values to use for P1, P2, and P3, given
   // mult_budget.
   int P1 = 1; // replace this with your optimized value
   int P2 = 1; // replace this with your optimized value
   int P3 = 1; // replace this with your optimized value

   // output top-level module
   // set your top-level name to "network_top"
   os << "module " << modName << "();" << endl;
   os << "   // this module should instantiate three subnetworks and wire them together" << endl;
   os << "endmodule" << endl;

   // -------------------------------------------------------------------------
   // Split up constVector for the three layers
   // layer 1's W matrix is M1 x N and its B vector has size M1
   int start = 0;
   int stop = M1*N+M1;
   vector<int> constVector1(&constVector[start], &constVector[stop]);

   // layer 2's W matrix is M2 x M1 and its B vector has size M2
   start = stop;
   stop = start+M2*M1+M2;
   vector<int> constVector2(&constVector[start], &constVector[stop]);

   // layer 3's W matrix is M3 x M2 and its B vector has size M3
   start = stop;
   stop = start+M3*M2+M3;
   vector<int> constVector3(&constVector[start], &constVector[stop]);

   if (stop > constVector.size()) {
      cout << "ERROR: constVector does not contain enough data for the requested design" << endl;
      cout << "The design parameters requested require " << stop << " numbers, but the provided data only have " << constVector.size() << " constants" << endl;
      assert(false);
   }
   // --------------------------------------------------------------------------


   // generate the three layer modules
   string subModName = "layer1_" + to_string(M1) + "_" + to_string(N) + "_" + to_string(P1) + "_" + to_string(bits);
   genLayer(M1, N, P1, bits, constVector1, subModName, os);

   subModName = "layer2_" + to_string(M2) + "_" + to_string(M1) + "_" + to_string(P2) + "_" + to_string(bits);
   genLayer(M2, M1, P2, bits, constVector2, subModName, os);

   subModName = "layer3_" + to_string(M3) + "_" + to_string(M2) + "_" + to_string(P3) + "_" + to_string(bits);
   genLayer(M3, M2, P3, bits, constVector3, subModName, os);

   // You will need to add code in the module at the top of this function to stitch together insantiations of these three modules

}


void printUsage() {
  cout << "Usage: ./gen MODE ARGS" << endl << endl;

  cout << "   Mode 1: Produce one neural network layer and testbench (Part 1 and Part 2)" << endl;
  cout << "      ./gen 1 M N P bits const_file" << endl;
  cout << "      Example: produce a neural network layer with a 4 by 5 matrix, with parallelism 1" << endl;
  cout << "               and 16 bit words, with constants stored in file const.txt" << endl;
  cout << "                   ./gen 1 4 5 1 16 const.txt" << endl << endl;

  cout << "   Mode 2: Produce a system with three interconnected layers with four testbenches (Part 3)" << endl;
  cout << "      Arguments: N, M1, M2, M3, mult_budget, bits, const_file" << endl;
  cout << "         Layer 1: M1 x N matrix" << endl;
  cout << "         Layer 2: M2 x M1 matrix" << endl;
  cout << "         Layer 3: M3 x M2 matrix" << endl;
  cout << "              e.g.: ./gen 2 4 5 6 7 15 16 const.txt" << endl << endl;
}
