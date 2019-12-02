#!/bin/bash
types=(1140
1310_pol
1310_zk
1310_zv
1320
1330_da
1330_hpr
2110
2120
2130_had
2130_hd
2150
2160
2170
2180
2190_mp
2190_overig
2310
2330
2330_bu
2330_dw
3270
4010
4030
rbbsg
rbbsm
5130
6120
6210_hk
6210_sk
6230
6230_ha
6230_hmo
6230_hn
6230_hnk
6410_mo
6410_ve
6430_bz
6430_hf
6430_hw
6430_mr
6510_hu
6510_hua
6510_huk
6510_hus
rbbha
rbbhc
rbbhf
rbbhfl
rbbkam
rbbvos
rbbzil
7140_base
7140_meso
7140_mrd
7140_oli
7150
7210
7230
rbbmc
rbbmr
rbbms
9110
9120
9130_end
9130_fm
9150
9160
9190
91E0
91E0_sf
91E0_va
91E0_vc
91E0_vm
91E0_vn
91E0_vo
91F0
rbbppm
rbbsf
rbbso
rbbsp)

# summing the max_phab values (res 32) per cell at current resolution (256):
for type in "${types[@]}" ; do
if [ `pgrep -c r.resamp.stats` -lt 4 ] ; then
       echo "Started summing for type '${type}' ..."
       r.resamp.stats input=max_${type} output=lev3_summax_${type} method=sum --overwrite &
   else
       echo "Started summing for type '${type}' ..."
       r.resamp.stats input=max_${type} output=lev3_summax_${type} method=sum --overwrite
   fi
done
wait


