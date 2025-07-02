import wasm from 'vite-plugin-wasm';

export default {
  plugins: [wasm()],
  base: '/encoins-v2/ui/',
  root: '.',
  build: {
    outDir: 'dist',
    target: 'esnext'
  }
};
