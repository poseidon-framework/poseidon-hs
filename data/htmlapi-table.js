document.addEventListener("DOMContentLoaded", function () {
  const table = document.querySelector("#currentTable");
  if (!table) return;

  new simpleDatatables.DataTable(table, {
    searchable: true,
    sortable: false,
    perPage: 10
  });
});
