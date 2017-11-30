// ESE 507 Project 3 Handout Code
// Fall 2017
// Peter Milder

// Getting started:
// The main() function contains the code to read the parameters.
// For Parts 1 and 2, your code should be in the genLayer() function. Please
// also look at this function to see an example for how to create the ROMs.
//
// For Part 3, your code should be in the genAllLayers() function.

#define MAX(x, y) (x > y) ? x : y

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cstdlib>
#include <cstring>
#include <assert.h>
#include <algorithm>
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

   int M = atoi(argv[2]);
   int N = (mode == 2) ? atoi(argv[2]) : atoi(argv[3]);
   int P = atoi(argv[4]);
   int M1 = atoi(argv[3]);
   int M2 = atoi(argv[4]);
   int M3 = atoi(argv[5]);
   int bits = (mode == 2) ? atoi(argv[7]) : atoi(argv[5]);
   int mult_budget = atoi(argv[6]);

   string out_file;
   if (mode == 2)
      out_file = "network_" + to_string(N) + "_" + to_string(M1) + "_" + to_string(M2) + "_" + to_string(M3) + "_" + to_string(mult_budget) + "_" + to_string(bits) + ".sv";
   else
      out_file = "layer_" + to_string(M) + "_" + to_string(N) + "_" + to_string(P) + "_" + to_string(bits) + ".sv";

   string append_mac_cmd = "cat mac.sv >> " + out_file;
   string append_mem_cmd = "cat memory.sv >> " + out_file;

   system(append_mac_cmd.c_str());
   system(append_mem_cmd.c_str());

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
   os << "   parameter M = " << M << "," << endl;
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
   os << "   localparam MAT_W_SIZE = (M * N) / P;" << endl;
   os << "   localparam VEC_b_SIZE = M / P;" << endl;
   os << "   localparam VEC_x_SIZE = N;" << endl;
   os << "   localparam VEC_y_SIZE = M;" << endl;
   os << "   localparam MAT_W_ADDW = $clog2(MAT_W_SIZE);" << endl;
   os << "   localparam VEC_b_ADDW = $clog2(VEC_b_SIZE);" << endl;
   os << "   localparam VEC_x_ADDW = $clog2(VEC_x_SIZE+1);" << endl;
   os << "   localparam VEC_y_ADDW = $clog2(VEC_y_SIZE+1);" << endl;
   os << endl;

   //Generating Internal Variables.
   os << "   enum logic [1:0] {GET_x=0, COMPUTE_y=1} state, next_state;" << endl;
   os << "   logic        [MAT_W_ADDW-1:0] rom_w_rd_addr[P];" << endl;
   os << "   logic        [VEC_b_ADDW-1:0] rom_b_rd_addr[P];" << endl;
   os << "   logic        [VEC_x_ADDW-1:0] ram_x_addr[P];" << endl;
   os << "   logic        [VEC_x_ADDW-1:0] ram_x_wr_addr;" << endl;
   os << "   logic        [VEC_x_ADDW-1:0] ram_x_rd_addr[P];" << endl;
   os << "   logic signed          [T-1:0] rom_w_data_out[P];" << endl;
   os << "   logic                 [T-1:0] rom_b_data_out[P];" << endl;
   os << "   logic signed          [T-1:0] ram_x_data_out[P];" << endl;
   os << "   logic signed          [T-1:0] mac_data_out[P];" << endl;
   os << "   logic                         ram_x_wr_en;" << endl;
   os << "   logic        [VEC_x_ADDW-1:0] vec_cnt[P];" << endl;
   os << "   logic                         next_req[P];" << endl;
   os << "   logic                         mac_valid_in[P];" << endl;
   os << "   logic                         mac_valid_out[P];" << endl;
   os << "   logic                         valid_int[P];" << endl;
   os << "   logic        [VEC_y_ADDW-1:0] output_cnt;" << endl;
   os << "   logic        [VEC_y_ADDW-1:0] vld_out_cnt;" << endl;  //TODO: change this to log(P)
   os << "   logic        [VEC_y_ADDW-1:0] vld_in_cnt[P];" << endl;  //TODO: change this to log(P)
   os << "   logic                         compute_done;" << endl;
   os << endl;

   //Generating Assign Statements
   os << "   assign s_ready = (state == GET_x);" << endl;
   os << "   assign ram_x_wr_en = s_ready & s_valid;" << endl;
   os << endl;

   os << "   always_comb begin" << endl;
   os << "       data_out   = 'bx;" << endl;
   os << "       m_valid    = 'bx;" << endl;
   os << "       for(int i = 0; i < P; i++) begin" << endl;
   os << "           ram_x_addr[i] = (state == GET_x) ? ram_x_wr_addr : ram_x_rd_addr[i];" << endl;
   os << "           if(vld_out_cnt == i) begin" << endl;
   os << "               data_out = (mac_data_out[i] < $signed(0)) ? 0 : mac_data_out[i];" << endl;
   os << "               m_valid  = valid_int[i];" << endl;
   os << "           end" << endl;
   os << "       end" << endl;
   os << "   end" << endl;
   os << endl;

   //Generating instantiations.
   string romModName;
   for(int i = 0; i < P; i++) {
      //Vec x memory
      os << "   memory #(" << endl;
      os << "      .WIDTH    ( T                     )," << endl;
      os << "      .SIZE     ( VEC_x_SIZE            )," << endl;
      os << "      .LOGSIZE  ( VEC_x_ADDW            ))" << endl;
      os << "   u_vec_x_mem_" << i << " (" << endl;
      os << "      .clk      ( clk                   )," << endl;
      os << "      .data_in  ( data_in               )," << endl;
      os << "      .data_out ( ram_x_data_out[" << i << "]     )," << endl;
      os << "      .addr     ( ram_x_addr[" << i << "]         )," << endl;
      os << "      .wr_en    ( ram_x_wr_en           ));" << endl;
      os << endl;

      //MAT a rom
      romModName = modName + "_W_rom_" + to_string(i);
      os << romModName << " u_w_rom_" << i << " (" << endl;
      os << "       .clk     ( clk                   )," << endl;
      os << "       .addr    ( rom_w_rd_addr[" << i << "]      )," << endl;
      os << "       .z       ( rom_w_data_out[" << i << "]     ));" << endl;
      os << endl;

      //Vec b rom
      romModName = modName + "_B_rom_" + to_string(i);
      os << romModName << " u_b_rom_" << i << " (" << endl;
      os << "       .clk     ( clk                   )," << endl;
      os << "       .addr    ( rom_b_rd_addr[" << i << "]      )," << endl;
      os << "       .z       ( rom_b_data_out[" << i << "]     ));" << endl;
      os << endl;

      //MAC
      os << "   part3_mac #(" << endl;
      os << "      .T         ( T                    )," << endl;
      os << "      .NUM_S     ( 1                    )," << endl; //TODO: change this to param.
      os << "      .VEC_S     ( VEC_x_SIZE           ))" << endl;
      os << "   u_mac_" << i << " (" << endl;
      os << "      .clk       ( clk                  )," << endl;
      os << "      .reset     ( reset                )," << endl;
      os << "      .a         ( rom_w_data_out[" << i << "]    )," << endl;
      os << "      .b         ( rom_b_data_out[" << i << "]    )," << endl;
      os << "      .x         ( ram_x_data_out[" << i << "]    )," << endl;
      os << "      .valid_in  ( mac_valid_in[" << i << "]      )," << endl;
      os << "      .f         ( mac_data_out[" << i << "]      )," << endl;
      os << "      .valid_out ( mac_valid_out[" << i << "]     )," << endl;
      os << "      .overflow  ( /* Not Used */       ));" << endl;
      os << endl;
   }

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
   //os << "             if(compute_done && m_valid && m_ready)" << endl;
   os << "             if(compute_done)" << endl;
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

   for(int i = 0; i < P; i++) {
      os << "   always_ff @(posedge clk)" << endl;
      os << "      if(reset) begin" << endl;
      os << "         rom_w_rd_addr[" << i << "] <= 'd0;" << endl;
      os << "         rom_b_rd_addr[" << i << "] <= 'd0;" << endl;
      os << "         ram_x_rd_addr[" << i << "] <= 'd0;" << endl;
      os << "      end" << endl;
      os << "      else begin" << endl;
      os << "         if(rom_w_rd_addr[" << i << "] == MAT_W_SIZE-1 && next_req[" << i << "]) begin" << endl;
      os << "            ram_x_rd_addr[" << i << "] <= 'd0;" << endl;
      os << "            rom_w_rd_addr[" << i << "] <= 'd0;" << endl;
      os << "            rom_b_rd_addr[" << i << "] <= 'd0;" << endl;
      os << "         end" << endl;
      os << "         else if (ram_x_rd_addr[ " << i << "] == VEC_x_SIZE-1 && next_req[" << i << "]) begin" << endl;
      os << "            ram_x_rd_addr[" << i << "] <= 'd0;" << endl;
      os << "            rom_w_rd_addr[" << i << "] <= rom_w_rd_addr[" << i << "] + 1'b1;" << endl;
      os << "            rom_b_rd_addr[" << i << "] <= rom_b_rd_addr[" << i << "] + 1'b1;" << endl;
      os << "         end" << endl;
      os << "         else if ((state == COMPUTE_y) && vec_cnt[" << i << "] < VEC_x_SIZE && next_req[" << i << "]) begin" << endl;
      os << "            ram_x_rd_addr[" << i << "] <= ram_x_rd_addr[" << i << "] + 1'b1;" << endl;
      os << "            rom_w_rd_addr[" << i << "] <= rom_w_rd_addr[" << i << "] + 1'd1;" << endl;
      os << "         end" << endl;
      os << "      end" << endl;
      os << endl;
   }

   os << "   always_ff @(posedge clk)" << endl;
   os << "      if(reset) begin" << endl;
   os << "         output_cnt    <= 'd0;" << endl;
   os << "         compute_done  <= 'd0;" << endl;
   os << "      end" << endl;
   os << "      else begin" << endl;
   os << "         if (state == GET_x) begin" << endl;
   os << "            output_cnt   <= 'd0;" << endl;
   os << "            compute_done  <= 'd0;" << endl;
   os << "         end" << endl;
   os << "         else if(output_cnt == M) begin" << endl;
   os << "            compute_done <= 'd1;" << endl;
   os << "         end" << endl;
   os << "         else if ((state == COMPUTE_y) && m_valid && m_ready) begin" << endl;
   os << "            output_cnt   <= output_cnt + 1'b1;" << endl;
   os << "         end" << endl;
   os << "      end" << endl;
   os << endl;

   for (int i = 0; i < P; i++) {
      os << "   always_ff @(posedge clk)" << endl;
      os << "      if(reset) begin" << endl;
      os << "         next_req[" << i << "]     <= 1'b1;" << endl;
      os << "         mac_valid_in[" << i << "] <= 1'b0;" << endl;
      os << "         vec_cnt[" << i << "]      <=  'd0;" << endl;
      os << "         vld_in_cnt[" << i << "]   <=  'd0;" << endl;
      os << "      end" << endl;
      os << "      else begin" << endl;
      os << "         if(compute_done) begin" << endl;
      os << "            vld_in_cnt[" << i << "]   <=  'd0;" << endl;
      os << "            next_req[" << i << "]     <= 1'b1;" << endl;
      os << "         end" << endl;
      os << "         else if(vld_in_cnt[" << i << "] == VEC_b_SIZE) begin" << endl;
      os << "            next_req[" << i << "]      <= 1'b0;" << endl;
      os << "            mac_valid_in[" << i << "]  <= 1'b0;" << endl;
      os << "            vec_cnt[" << i << "]       <= 2'd0;" << endl;
      os << "         end" << endl;
      os << "         else if(vec_cnt[" << i << "] == VEC_x_SIZE) begin" << endl;
      os << "            next_req[" << i << "]      <= 1'b0;" << endl;
      os << "            mac_valid_in[" << i << "]  <= 1'b0;" << endl;
      os << "            vec_cnt[" << i << "]       <= 2'd0;" << endl;
      os << "            vld_in_cnt[" << i << "]    <= vld_in_cnt[" << i << "] + 1'b1;" << endl;
      os << "         end" << endl;
      os << "         else if (m_valid && m_ready && (vld_out_cnt == " << i << ") && (vld_in_cnt[" << i <<"] < VEC_b_SIZE)) begin" << endl;
      os << "            next_req[" << i << "]      <= 1'b1;" << endl;
      os << "         end" << endl;
      os << "         else if (next_req[" << i << "] && (state == COMPUTE_y)) begin" << endl;
      os << "            next_req[" << i << "]      <= 1'b1;" << endl;
      os << "            mac_valid_in[" << i << "]  <= 1'b1;" << endl;
      os << "            vec_cnt[" << i << "]       <= vec_cnt[" << i << "] + 1'b1;" << endl;
      os << "         end" << endl;
      os << "      end" << endl;
      os << endl;
   }

   os << "   always_ff @(posedge clk)" << endl;
   os << "       if(reset) begin" << endl;
   os << "           vld_out_cnt <= 'd0;" << endl;
   os << "       end" << endl;
   os << "       else begin" << endl;
   os << "           if(vld_out_cnt == P-1 && m_valid && m_ready) begin" << endl;
   os << "               vld_out_cnt <= 'd0;" << endl;
   os << "           end" << endl;
   os << "           else if(m_valid & m_ready) begin" << endl;
   os << "               vld_out_cnt <= vld_out_cnt + 1'b1;" << endl;
   os << "           end" << endl;
   os << "       end" << endl;
   os << endl;

   os << "   always_ff @(posedge clk)" << endl;
   os << "      if(reset) begin" << endl;
   os << "         for(int i = 0; i < P; i++) begin" << endl;
   os << "            valid_int[i] <= 1'b0;" << endl;
   os << "         end" << endl;
   os << "      end" << endl;
   os << "      else begin" << endl;
   os << "         if(m_valid && m_ready) begin" << endl;
   os << "            for(int i = 0; i < P; i++) begin" << endl;
   os << "               if(vld_out_cnt == i) valid_int[i] <= 1'b0;" << endl;
   os << "               else                 valid_int[i] <= (mac_valid_out[i]) ? 1'b1 : valid_int[i];" << endl;
   os << "            end" << endl;
   os << "         end" << endl;
   os << "         else begin" << endl;
   os << "            for(int i = 0; i < P; i++) begin" << endl;
   os << "               valid_int[i] <= (mac_valid_out[i]) ? 1'b1 : valid_int[i];" << endl;
   os << "            end" << endl;
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

   for(int i = 0; i < P; i++) {
      // Generate a ROM (for W) with constants 0 through M*N-1, with "bits" number of bits
      romModName = modName + "_W_rom_" + to_string(i);
      vector<int> wVector;
      for(int j = (i*N); j < (M*N); j+=(N*P)) {
         for(int k = 0; k < N; k++) wVector.push_back(constVector[j+k]);
      }
      genROM(wVector, bits, romModName, os);

      // Generate a ROM (for B) with constants M*N through M*N+M-1 wits "bits" number of bits
      romModName = modName + "_B_rom_" + to_string(i);
      vector<int> bVector;
      for(int j = i; j < M; j+=P) {
         bVector.push_back(constVector[(M*N)+j]);
      }
      genROM(bVector, bits, romModName, os);
   }

}

int mult_required(int lsize, int lparam) {
   int mult_req = 1;

   if(lsize % 2 != 0) {
      mult_req = lsize-1;
   }
   else {
      while(lsize % (mult_req+lparam) != 0) {
        mult_req++;
      }
   }

   return mult_req;
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

   int layer_params[3] = {1, 1, 1};
   int layer_size[3] = {M1, M2, M3};
   int layer_done[3] = {0, 0, 0};
   int orig_cost[3] = {N*M1, M1*M2, M2*M3};

   vector<int> layer_cost;

   //Initial cost of each layer.
   layer_cost.push_back( N*M1);
   layer_cost.push_back(M1*M2);
   layer_cost.push_back(M2*M3);

   //Assuming all three layer working on continous data, the throughput of the
   //whole pipeline will be limited by the layer with most multiplication. We
   //tread that as the cost of whole system. I use a greedy algorithm which assign
   //available multiplier to the layer which decreases the cose of the pipeline most.

   int budget = mult_budget - 3; //3 multipliers are the minimum we need.

   while(budget) {
     auto max_cost = *std::max_element(layer_cost.begin(), layer_cost.end());
     auto it = std::find(layer_cost.begin(), layer_cost.end(), max_cost);
     auto idx = distance(layer_cost.begin(), it);

     //cout << "Budget: " << budget << " Idx: " << idx << endl;

     int mult_req = mult_required(layer_size[idx], layer_params[idx]);

     if(budget >= mult_req && mult_req != 0) {
        budget -= mult_req;
        layer_params[idx] += mult_req;
        layer_cost[idx] = orig_cost[idx] / layer_params[idx];
        if(layer_params[idx] == layer_size[idx]) {
          layer_cost[idx] = 0;
          layer_done[idx] = 1;
        }
     }
     else {
        layer_cost[idx] = 0;
        layer_done[idx] = 1;
     }

     if(layer_done[0] && layer_done[1] && layer_done[2]) break;
     //cout << "Mult Req: " << mult_req << " P: " << layer_params[idx] << endl;

   }

   int P1 = layer_params[0];
   int P2 = layer_params[1];
   int P3 = layer_params[2];

   //Generating File Header
   os << "// ------------------------------------------//" << endl;
   os << "// Neural Network -- Part 3                    " << endl;
   os << "// ------------------------------------------//" << endl;
   os << "// NAME:  Atif Iqbal                           " << endl;
   os << "// NETID: aahangar                             " << endl;
   os << "// SBUID: 111416569                            " << endl;
   os << "// ------------------------------------------//" << endl;
   os << endl << endl;

   //Generating Module Header
   os << "module " << modName << " #(" << endl;
   os << "   parameter M1 = " << M1 << "," << endl;
   os << "   parameter M2 = " << M2 << "," << endl;
   os << "   parameter M3 = " << M3 << "," << endl;
   os << "   parameter N  = " << N  << "," << endl;
   os << "   parameter P1 = " << P1 << "," << endl;
   os << "   parameter P2 = " << P2 << "," << endl;
   os << "   parameter P3 = " << P3 << "," << endl;
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

   os << "   logic                   ms_ready_1;" << endl;
   os << "   logic                   sm_valid_1;" << endl;
   os << "   logic signed [T-1:0]    sm_data_out_1;" << endl;
   os << "   logic                   ms_ready_2;" << endl;
   os << "   logic                   sm_valid_2;" << endl;
   os << "   logic signed [T-1:0]    sm_data_out_2;" << endl;
   os << endl;

   os << "   layer1_" << M1 << "_" << N << "_" << P1 << "_" << bits << " #(" << endl;
   os << "      .M         ( " << M1 << "         )," << endl;
   os << "      .N         ( " << N << "          )," << endl;
   os << "      .P         ( " << P1 << "         )," << endl;
   os << "      .T         ( " << bits << "       ))" << endl;
   os << "   u_layer1_" << M1 << "_" << N << "_" << P1 << "_" << bits << " (" << endl;
   os << "      .clk       ( clk                  )," << endl;
   os << "      .reset     ( reset                )," << endl;
   os << "      .s_valid   ( s_valid              )," << endl;
   os << "      .data_in   ( data_in              )," << endl;
   os << "      .s_ready   ( s_ready              )," << endl;
   os << "      .m_ready   ( ms_ready_1           )," << endl;
   os << "      .m_valid   ( sm_valid_1           )," << endl;
   os << "      .data_out  ( sm_data_out_1        ));" << endl;
   os << endl;

   os << "   layer2_" << M2 << "_" << M1 << "_" << P2 << "_" << bits << " #(" << endl;
   os << "      .M         ( " << M2 << "         )," << endl;
   os << "      .N         ( " << M1 << "         )," << endl;
   os << "      .P         ( " << P2 << "         )," << endl;
   os << "      .T         ( " << bits << "       ))" << endl;
   os << "   u_layer2_" << M2 << "_" << M1 << "_" << P2 << "_" << bits << " (" << endl;
   os << "      .clk       ( clk                  )," << endl;
   os << "      .reset     ( reset                )," << endl;
   os << "      .s_ready   ( ms_ready_1           )," << endl;
   os << "      .s_valid   ( sm_valid_1           )," << endl;
   os << "      .data_in   ( sm_data_out_1        )," << endl;
   os << "      .m_ready   ( ms_ready_2           )," << endl;
   os << "      .m_valid   ( sm_valid_2           )," << endl;
   os << "      .data_out  ( sm_data_out_2        ));" << endl;
   os << endl;

   os << "   layer3_" << M3 << "_" << M2 << "_" << P3 << "_" << bits << " #(" << endl;
   os << "      .M         ( " << M3 << "         )," << endl;
   os << "      .N         ( " << M2 << "         )," << endl;
   os << "      .P         ( " << P3 << "         )," << endl;
   os << "      .T         ( " << bits << "       ))" << endl;
   os << "   u_layer3_" << M3 << "_" << M2 << "_" << P3 << "_" << bits << " (" << endl;
   os << "      .clk       ( clk                  )," << endl;
   os << "      .reset     ( reset                )," << endl;
   os << "      .s_ready   ( ms_ready_2           )," << endl;
   os << "      .s_valid   ( sm_valid_2           )," << endl;
   os << "      .data_in   ( sm_data_out_2        )," << endl;
   os << "      .m_ready   ( m_ready              )," << endl;
   os << "      .m_valid   ( m_valid              )," << endl;
   os << "      .data_out  ( data_out             ));" << endl;
   os << endl;

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
