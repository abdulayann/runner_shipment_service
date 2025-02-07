package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.TiTruckDriverDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface ITiTruckDriverDetailDao {
    List<TiTruckDriverDetails> saveAll(List<TiTruckDriverDetails> tiTruckDriverDetailsList);
    TiTruckDriverDetails save(TiTruckDriverDetails tiTruckDriverDetails);
    Page<TiTruckDriverDetails> findAll(Specification<TiTruckDriverDetails> spec, Pageable pageable);
    Optional<TiTruckDriverDetails> findById(Long id);
    void delete(TiTruckDriverDetails tiTruckDriverDetails);
    List<TiTruckDriverDetails> findByIds(List<Long> id);
}
