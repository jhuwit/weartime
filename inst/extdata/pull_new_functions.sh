cd inst/extdata
rm -rf CNN-Non-Wear-Time-Algorithm
git clone --depth=1 https://github.com/muschellij2/CNN-Non-Wear-Time-Algorithm CNN-Non-Wear-Time-Algorithm
rm -rf cnn
mkdir -p cnn
mkdir -p cnn/functions/
cp CNN-Non-Wear-Time-Algorithm/functions/*.py cnn/functions
rm -f cnn/functions/gt3x_functions.py
rm -rf CNN-Non-Wear-Time-Algorithm
cd ../../
