## Data

preprocessing data is as described in the original readme file. I have added a tiny dir in the data dir which contains a tiny set of data from the original corpus (which can be downloaded at their provided link) just for pipeline testing.

## Training of compound PCFG

If using a CPU :

python train.py --train_file data/tiny/ptb-train.pkl --val_file data/tiny/ptb-val.pkl --save_path data/tiny/compound-pcfg.pt --num_epochs 5 --print_every 20

If using a GPU :

python train.py --train_file data/tiny/ptb-train.pkl --val_file data/tiny/ptb-val.pkl --save_path data/tiny/compound-pcfg.pt --device_type gpu --gpu 0 --num_epochs 5 --print_every 20

where the gpu 0 is your default gpu in cluster.
For more on the flags and their defaults, check the train.py file.
