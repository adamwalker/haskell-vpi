`timescale 1ns/10ps

module main;

    //clock and reset signals
	reg clk = 0;
    reg reset_n = 0;

    //data signal that will be set through the VPI
    reg [31:0] data = 0;

    //generate the clock
	always #1 clk = ~clk;

    //generate the reset signal
    initial
    begin
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);
        reset_n = 1;
    end

    //dump the signal waveforms
    initial
    begin
        $dumpfile("wave.vcd");
        $dumpvars(0, clk, reset_n, data);
    end

    //run for 100 cycles
    initial
    begin
        #100;
        $finish;
    end

    //call a function to set the data register through the VPI
    always @(posedge clk)
    begin
        $setdata;
    end

    //call a function to print the data register through the VPI
    always @(posedge clk)
    begin
        $printsigs;
    end

endmodule
