#include <Rcpp.h>
using namespace Rcpp;


// Fonction pour calculer la somme cumulative d'un vecteur booléen
// Equivalent pour la fonction cumsum() dans R
NumericVector cumulative_sum(LogicalVector cond) {
  int n = cond.size();
  NumericVector result(n);

  if (n > 0) {
    result[0] = cond[0] ? 1 : 0;
    for (int i = 1; i < n; ++i) {
      result[i] = result[i - 1] + (cond[i] ? 1 : 0);
    }
  }

  return result;
}


// [[Rcpp::export]]
List get_mzmat_eic_cpp(NumericVector intensity_values,
                       NumericVector mz_values,
                       NumericVector scanindex_values,
                       NumericVector scantime_values,
                       NumericVector mz_range) {

  // Trouver les indices des mz_values qui se trouvent dans la plage mz_range
  LogicalVector ids = (mz_values >= mz_range[0]) & (mz_values <= mz_range[1]);

  // Initialiser un vecteur pour stocker les indices des scans correspondants
  NumericVector scans(ids.size());

  // Itérer sur les ids pour trouver les indices des scans correspondants
  for (int i = 0; i < ids.size(); ++i) {
    if (ids[i]) {
      double id = mz_values[i];
      // Trouver l'indice du scan correspondant à l'id
      scans[i] = scanindex_values[which_min(abs(scanindex_values - id))];
    }
  }

  // Créer la matrice mzmat à partir des scans, mz_values et intensity_values
  int n = sum(ids); // Nombre de lignes dans la matrice mzmat
  NumericMatrix mzmat(n, 3);

  for (int i = 0, j = 0; i < ids.size(); ++i) {
    if (ids[i]) {
      mzmat(j, 0) = scans[i];
      mzmat(j, 1) = mz_values[i];
      mzmat(j, 2) = intensity_values[i];
      ++j;
    }
  }

  // Créer la matrice eic avec une colonne pour les numéros de scan (initialisée avec scantime_values)
  NumericMatrix eic(scantime_values.size(), 2);
  colnames(eic) = CharacterVector::create("scan", "int");

  for (int i = 0; i < scantime_values.size(); ++i) {
    eic(i, 0) = i + 1; // scan number
    eic(i, 1) = 0;      // intensity (initially zero)
  }

  // Agréger les intensités par scan dans la matrice eic
  for (int i = 0; i < n; ++i) {
    int scan = mzmat(i, 0) - 1; // convertir en indice (base 0)
    eic(scan, 1) += mzmat(i, 2); // sum intensity for each scan
  }

  // Ajouter les temps de rétention (scantime_values) à la matrice eic
  NumericVector rt = clone(scantime_values);
  colnames(eic) = CharacterVector::create("scan", "int");

  // Retourner les matrices mzmat et eic dans une liste
  return List::create(Named("mzmat") = mzmat,
                      Named("eic") = eic);
}


// [[Rcpp::export]]
IntegerVector get_rois_cpp(NumericVector ints, int min_width, int missing_scans = 1) {
  int n = ints.size();

  // Calculer le baseline avec R runmed
  Function runmed("runmed");
  NumericVector baseline = runmed(ints, n / 3, Named("endrule") = "constant", Named("algorithm") = "Turlach");

  // Déterminer les indices des ROIs
  LogicalVector rois = (ints - baseline) > 0;

  if (sum(rois) == 0) {
    return IntegerVector(); // Retourner un vecteur vide si aucun ROI détecté
  }

  // Séparer les indices des ROIs en groupes de scans consécutifs
  std::vector<std::vector<int>> roi_groups;
  std::vector<int> current_roi_group;

  for (int i = 0; i < n; ++i) {
    if (rois[i]) {
      current_roi_group.push_back(i);
    } else {
      if (!current_roi_group.empty()) {
        roi_groups.push_back(current_roi_group);
        current_roi_group.clear();
      }
    }
  }

  if (!current_roi_group.empty()) {
    roi_groups.push_back(current_roi_group);
  }

  // Filtrer les groupes de ROIs en fonction de la largeur minimale
  std::vector<std::vector<int>> filtered_roi_groups;

  for (const auto& roi_group : roi_groups) {
    if (roi_group.size() >= min_width) {
      filtered_roi_groups.push_back(roi_group);
    }
  }

  if (filtered_roi_groups.empty()) {
    return IntegerVector(); // Retourner un vecteur vide si aucun ROI suffisamment large
  }

  // Sélectionner le ROI avec l'intensité maximale
  int max_intensity_index = std::distance(ints.begin(), std::max_element(ints.begin(), ints.end()));

  for (size_t i = 0; i < filtered_roi_groups.size(); ++i) {
    if (std::find(filtered_roi_groups[i].begin(), filtered_roi_groups[i].end(), max_intensity_index) != filtered_roi_groups[i].end()) {
      // Retourner les bornes du ROI sélectionné
      return IntegerVector::create(filtered_roi_groups[i].front() + 1, filtered_roi_groups[i].back() + 1);
    }
  }

  return IntegerVector(); // Retourner un vecteur vide par défaut
}


// [[Rcpp::export]]
List overlap_rois_cpp(List rois) {
  int n = rois.size();

  if (n < 2) {
    return rois; // Retourner directement la liste rois si elle contient moins de 2 éléments
  }

  List rois_final = List::create(rois[0]); // Initialiser rois_final avec le premier élément de rois

  // Parcourir les ROIs restants dans rois et les fusionner avec rois_final si nécessaire
  for (int i = 1; i < n; ++i) {
    List roi = rois[i];
    bool merged = false;

    // Extraire les bornes du ROI actuel et du ROI en cours de fusion
    int roi_start = as<int>(roi[0]);
    int roi_end = as<int>(roi[1]);

    // Parcourir les ROIs déjà fusionnés dans rois_final
    for (int j = 0; j < rois_final.size(); ++j) {
      List current_roi = rois_final[j];

      // Extraire les bornes du ROI déjà fusionné
      int current_start = as<int>(current_roi[0]);
      int current_end = as<int>(current_roi[1]);

      // Vérifier s'il y a un chevauchement entre les deux ROIs
      if (current_start <= roi_end && current_end >= roi_start) {
        // Mettre à jour les bornes du ROI existant pour inclure le nouveau ROI
        current_roi[0] = std::min(current_start, roi_start);
        current_roi[1] = std::max(current_end, roi_end);
        rois_final[j] = current_roi;
        merged = true;
        break;
      }
    }

    // Si aucun chevauchement n'a été trouvé, ajouter le nouveau ROI à rois_final
    if (!merged) {
      rois_final.push_back(roi);
    }
  }

  return rois_final;
}



// [[Rcpp::export]]
IntegerVector descend_min_cpp(NumericVector y, int center, int minPts = 1) {
  int n = y.size();

  // Trouver l'indice du premier point de descente minimal à gauche
  int left = center;
  if (center != 0) {
    IntegerVector lefts;
    for (int i = center - 1; i >= 0; --i) {
      if (y[i + 1] < y[i]) {
        lefts.push_back(i);
      } else {
        break;
      }
    }
    if (lefts.size() > minPts + 1) {
      left = lefts[minPts + 1];
    } else {
      left = 0;
    }
  }

  // Trouver l'indice du premier point de descente minimal à droite
  int right = center;
  if (center != n - 1) {
    IntegerVector rights;
    for (int i = center + 1; i < n; ++i) {
      if (y[i - 1] < y[i]) {
        rights.push_back(i);
      } else {
        break;
      }
    }
    if (rights.size() > minPts + 1) {
      right = rights[minPts + 1];
    } else {
      right = n - 1;
    }
  }

  // Retourner les indices gauche et droit des points de descente minimaux
  return IntegerVector::create(left + 1, right + 1); // Ajouter 1 pour convertir de 0-indexed à 1-indexed
}


// [[Rcpp::export]]
IntegerVector narrow_rt_boundaries_extend_cpp(IntegerVector xrange, int center, NumericVector y, int minPts = 1) {
  int n = y.size();
  int left = xrange[0];
  int right = xrange[1];

  // Trouver le nouveau bornage min à gauche
  if (xrange[0] != 0) {
    IntegerVector lefts;
    for (int i = xrange[0] - 1; i >= 0; --i) {
      if (y[i] <= 0) {
        lefts.push_back(i);
      } else {
        break;
      }
    }

    // Calculer les limites des plages continues d'indices
    int limit_left = 0;
    for (int j = 1; j < lefts.size(); ++j) {
      if (lefts[j - 1] != lefts[j] - 1) {
        if (j - limit_left > minPts + 1) {
          left = lefts[limit_left];
          break;
        }
        limit_left = j;
      }
    }
  }

  // Trouver le nouveau bornage max à droite
  if (xrange[1] != n - 1) {
    IntegerVector rights;
    for (int i = xrange[1] + 1; i < n; ++i) {
      if (y[i] <= 0) {
        rights.push_back(i);
      } else {
        break;
      }
    }

    // Calculer les limites des plages continues d'indices
    int limit_right = 0;
    for (int j = 1; j < rights.size(); ++j) {
      if (rights[j - 1] != rights[j] - 1) {
        if (j - limit_right > minPts + 1) {
          right = rights[limit_right];
          break;
        }
        limit_right = j;
      }
    }
  }

  // Retourner les nouveaux bornages min et max
  return IntegerVector::create(left + 1, right + 1); // Ajouter 1 pour convertir de 0-indexed à 1-indexed
}



// [[Rcpp::export]]
IntegerVector narrow_rt_boundaries_reduce_cpp(IntegerVector xrange, int center, NumericVector y, int minPts = 1) {
  int n = y.size();
  int left = xrange[0];
  int right = xrange[1];

  // Trouver le nouveau bornage min à gauche
  if (xrange[0] != center) {
    IntegerVector lefts;
    for (int i = xrange[0]; i <= center; ++i) {
      if (y[i] > 0) {
        lefts.push_back(i);
      }
    }

    // Calculer les limites des plages continues d'indices
    NumericVector cumsum = cumulative_sum(y[xrange[0] <= center]);
    IntegerVector limits;
    for (int j = xrange[0]; j <= center; ++j) {
      if (y[j] > 0 && (j == xrange[0] || y[j - 1] <= 0)) {
        limits.push_back(j);
      }
    }

    int limit = 0;
    for (int j = 0; j < limits.size(); ++j) {
      if (limits[j] - xrange[0] > minPts + 1) {
        limit = limits[j];
        break;
      }
    }
    left = limit == 0 ? xrange[0] : limits[limit] - 1;
  }

  // Trouver le nouveau bornage max à droite
  if (xrange[1] != center) {
    IntegerVector rights;
    for (int i = xrange[1]; i >= center; --i) {
      if (y[i] > 0) {
        rights.push_back(i);
      }
    }

    // Calculer les limites des plages continues d'indices
    NumericVector cumsum = cumulative_sum(y[center <= xrange[1]]);
    IntegerVector limits;
    for (int j = center; j <= xrange[1]; ++j) {
      if (y[j] > 0 && (j == center || y[j + 1] <= 0)) {
        limits.push_back(j);
      }
    }

    int limit = 0;
    for (int j = 0; j < limits.size(); ++j) {
      if (xrange[1] - limits[j] > minPts + 1) {
        limit = limits[j];
        break;
      }
    }
    right = limit == 0 ? xrange[1] : limits[limit] + 1;
  }

  // Retourner les nouveaux bornages min et max
  return IntegerVector::create(std::max(1, left), std::min(n, right)); // Limiter les bornages dans les limites valides
}
