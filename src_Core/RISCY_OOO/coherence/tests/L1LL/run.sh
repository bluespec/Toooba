
# Copyright (c) 2017 Massachusetts Institute of Technology
# 
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use, copy,
# modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#make
#if [ $? -ne 0 ]; then
#    exit $?
#fi

./simL1LL > log
grep "L1 .* tagMatch" log > L1TagMatch.log
grep "L1 .* cRqTransfer" log > L1CRqTransfer.log
grep "L1 .* pRqTransfer" log > L1PRqTransfer.log
grep "L1 .* pRsTransfer" log > L1PRsTransfer.log
grep "L1 .* sendRqToP" log > L1SendRqToP.log
grep "L1 .* pipelineResp" log > L1PipelineResp.log
grep "LL .* tagMatch" log > LLTagMatch.log
grep "LL .* cRqTransfer" log > LLCRqTransfer.log
grep "LL .* cRsTransfer" log > LLCRsTransfer.log
grep "LL .* mRsTransfer" log > LLMRsTransfer.log
grep "LL .* sendToM" log > LLSendToM.log
grep "LL .* sendRsToDma" log > LLSendRsToDma.log
grep "LL .* sendRsToC" log > LLSendRsToC.log
grep "LL .* sendRqToC" log > LLSendRqToC.log
grep "LL .* pipelineResp" log > LLPipelineResp.log
grep "XBAR:" log > XBar.log
tail log
