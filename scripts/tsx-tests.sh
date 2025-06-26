#!/bash/sh

for f in test/**/*.ts; do
  npx tsx "$f" || exit 1
done

echo "✅ All tests completed successfully."
exit 0
