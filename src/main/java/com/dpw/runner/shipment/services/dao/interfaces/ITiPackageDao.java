package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.TiPackages;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface ITiPackageDao {
    List<TiPackages> saveAll(List<TiPackages> tiPackagesList);
    TiPackages save(TiPackages tiPackages);
    Page<TiPackages> findAll(Specification<TiPackages> spec, Pageable pageable);
    Optional<TiPackages> findById(Long id);
    void delete(TiPackages tiPackages);
    List<TiPackages> findByIds(List<Long> id);
}
