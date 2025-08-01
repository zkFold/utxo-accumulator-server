import wasm from 'vite-plugin-wasm';

export default {
  plugins: [wasm()],
  base: '/encoins-v2/',
  root: '.',
  build: {
    outDir: 'dist',
    target: 'esnext'
  }
};
