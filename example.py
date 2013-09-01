## ## ## matplotlib_example.py ## ## ##
## ## ## Created by: KDP      ## ## ##
 
import numpy as np
import matplotlib.pyplot as plt
 
## Create data
x = np.linspace(0, 4 * np.pi, num=100)
y = np.sin(x)
 
## Save data
np.savez("mydata.npz", x=x, y=y)
 
## Load data
mydata = np.load("mydata.npz")
x = mydata["x"]
y = mydata["y"]
 
plt.plot(x, y, c="r", marker="o", label="Sine")
 
plt.savefig("matplotlib_example_1.png")
plt.show()
plt.clf() # Clear figure
 
## More advanced
x = np.linspace(0, 4 * np.pi, num=100)
y1 = np.sin(x)
y2 = np.cos(x)
 
fig = plt.figure(figsize=(8, 10), dpi=100)
 
ax1 = fig.add_subplot(211)
 
y1 = y1 + (np.random.randn(len(y1)) / 8.0)
 
ax1.scatter(x, y1, c="r", marker="d", label="Sine")
plt.title("Sine with randomness")
plt.legend(loc="upper right")
plt.xlim(-0.5 * np.pi, 4.5 * np.pi)
plt.ylim(-1.25, 1.25)
plt.ylabel("Phase")
plt.xlabel("Magnitude")
 
ax2 = fig.add_subplot(212)
 
y2 = y2 + (np.random.randn(len(y2)) / 8.0)
 
ax2.scatter(x, y2, c="b", marker="d", label="Cosine")
plt.title("Cosine with randomness")
plt.legend(loc="upper right")
plt.ylim(-1.25, 1.25)
plt.xlim(-0.5 * np.pi, 4.5 * np.pi)
plt.ylabel("Phase")
plt.xlabel("Magnitude")
 
plt.savefig("matplotlib_example_2.png")
plt.clf() # Clear figure
