import wasm from 'vite-plugin-wasm';

export default {
  plugins: [wasm()],
  root: '.',
  build: {
    outDir: 'dist',
    target: 'esnext'
  }
};
