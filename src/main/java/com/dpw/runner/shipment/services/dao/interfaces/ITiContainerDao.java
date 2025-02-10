package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.TiContainers;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface ITiContainerDao {

    List<TiContainers> saveAll(List<TiContainers> tiContainersList);
    TiContainers save(TiContainers tiContainers);
    Page<TiContainers> findAll(Specification<TiContainers> spec, Pageable pageable);
    Optional<TiContainers> findById(Long id);
    void delete(TiContainers tiContainers);
    List<TiContainers> findByIds(List<Long> id);
}
