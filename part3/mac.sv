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
