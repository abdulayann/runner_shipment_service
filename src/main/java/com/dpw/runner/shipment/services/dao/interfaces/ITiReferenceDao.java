package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.TiReferences;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface ITiReferenceDao {
    List<TiReferences> saveAll(List<TiReferences> tiReferencesList);

    TiReferences save(TiReferences tiReferences);

    Page<TiReferences> findAll(Specification<TiReferences> spec, Pageable pageable);

    Optional<TiReferences> findById(Long id);

    void delete(TiReferences tiReferences);

    List<TiReferences> findByIds(List<Long> id);
}
