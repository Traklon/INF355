
I - Introduction to Quantum Computing.


In quantum computing, Qubits are the equivalent of the bits that
can be used in classical computing. As the Schrödinger cat can be
dead and alive in the same time, Qubits can be both in the state 0
and 1 (respectively written "|0>" and "|1>").
More theoretically, a Qubit can be written in the form a*|O> + b*|1>
where a and b are complex numbers. When a measure is performed on
the qubit, the value is fixed to 0 with a probability |a|², and b
with a probability |b|² (the sum of the squares of the modulus being 1).

The definition can be extended to what we will call "Registers".
When we consider several Qubits in a certain order, we can either see
them separately : (a*|0> + b*|1>)(c*|0> + d*|1>), or as a whole :
ac*|00> + ad*|01> + bc*|10> + bd*|11>. Notice that the sum of squares
of the modulus is still 1.
Although going from the first notiation to the second is quite easy,
the opposite isn't (but getting the modulus is).
For this reason, the second notation will be used more, even if my
first goal was to design a quantum simulation that used the first one,
for obvious memory size reason (for 10 Qubits, only 20 complex numbers
need to be stored with the first notation, compared to 1024 with the
second one).

When we will consider a Register, and a fortiori a Qubit, we will
write its coordinates in the classical basis. For example, with 3
Qubits : {|000> ; |001> ; |010> ; |011> ; |100> ; |101> ; |110> ; |111>}

Operations on Registers are done with "Gates". They are the simulation
of machines that exist in the real world to use quantum mechanics.
Gates often use only one or two Qubits. However, since it is difficult
to extract them from the register, we "expand" them to take as argument
all the Qubits. 
In our implementation, Gates are represented with packed matrices. 

With a small set of gates, it is proven that any boolean function can
be created by chaining them. This mechanism is called a "Circuit".
Contrary to classical electrical circuits, these circuits can always be
read from left to right and can thus be implemented as a list of Gates.




II - The implementation.


The choices that have been made in our implementation are for the sake
of simplicity, modularity and performance. This work can be seen as the
construction of a framework for the simulation of quantum computing.
The functions that act on Gates, Circuits, Registers should be sufficient
to simulate lots of different circuits.

The examples that are described in the source code are sufficient to
understand in depth what has been done.

This basis can be used to go further than where I went because the 
possibilities are virtually infinite. The code seems to work up to
9 Qubits on a laptop, the utilisation of matrices being the bottleneck.

More information on the choices of implementation can be found directly
in the source code and/or will be explained during the presentation.
