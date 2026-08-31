This folder contains summaries of the 26B proposals, created with `itac summarize`, with PI names removed.

```bash
itac ls | cut -wf1 | grep -v ^Id | while IFS= read -r s; do
  echo $s
  itac summarize $s | grep -v ^PI: > summaries/$s.yaml
done
```

There are also files indicating the bands in which time was awarded. These had to be hand-corrected for filename mismatches.

```bash
for i in {1..4}; do
    echo "band $i"
    ls band-$i | cut -d. -f1 > band-$i.lst
done
```
