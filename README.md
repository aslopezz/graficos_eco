

app/
├── app.R
├── global.R
├── ui.R
├── server.R
└── R/
    ├── 01_cargar_datos.R > procesar_datos_entrada
    ├── 02_utils.R > map_colors, resumen_por_variable
    ├── 03_indices_diversidad.R > calcular_indices, tabla_riqueza_abundancia
    ├── 04_graficos.R > graficar, plot_curva_acumulacion
    ├── 05_curvas_acumulacion.R > calcular_curva_acumulacion
    └── 06_analisis_espacial.R > preparar_datos_espaciales, crear_shp_terreno
    
