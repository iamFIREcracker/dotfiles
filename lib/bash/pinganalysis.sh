pinganalyzer()
{
    for src; do
        echo ${src}.dat
        cat ${src} |
                awk 'NF==8 {print $7}' |
                sed s/.*time=// |
                awk '{print $1}' > ${src}.dat
    done
}

function tcpdumpanalyzer() {
    for src; do
        echo ${src}-request.dat
        cat ${src} | 
                tr , ' ' |
                awk 'NR==1 {first=$1; prev=-1}
                    /request/ {
                        if (prev == -1)
                            prev=$1;
                        print $12, $1 - first, $1 - prev;
                        prev=$1
                    }' > ${src}-request.dat
        echo ${src}-reply.dat
        cat ${src} | 
                tr , ' ' |
                awk 'NR==1 {first=$1; prev=-1}
                    /reply/ {
                        if (prev == -1)
                            prev=$1;
                        print $12, $1 - first, $1 - prev;
                        prev=$1
                    }' > ${src}-reply.dat
        echo ${src}-delta.dat
        cat ${src} |
                awk '/request/       { req[$12] =  $1}
                    /reply/         { reply[$12] = $1}

                    END {
                         for (i in reply) {
                             printf("%d %8.6f\n", i, reply[i] - req[i]);
                         }
                    }' |
                sort -n > ${src}-delta.dat
    done
}


function loopbackanalyzr() {
    for src; do
        echo ${src}.dat
        cat ${src} |
            awk 'NF==7{print ($2 - $1)/2934, ($3 - $2)/2934, ($4 - $3)/2934, ($5 - $4)/2934, ($6 - $5)/2934, ($7 - $6)/2934}' > ${src}.dat
    done
}

function loopbackcpuidanalyzr() {
    for src; do
        echo ${src}.dat
        cat ${src} |
            awk 'NF==14{print ($3 - $1)/2934, ($5 - $3)/2934, ($7 - $5)/2934, ($9 - $7)/2934, ($11 - $9)/2934, ($13 - $11)/2934}' > ${src}.dat
    done
}

function pktgenanalyzer() {
    for src; do
        echo ${src}.dat
        cat ${src} |
                awk 'NF==8{print $1/2934, $2/2934, $3/2934, $4/2934, $5/2934, $6/2934, $7/2934, $8/2934}' > ${src}.dat
    done
}
