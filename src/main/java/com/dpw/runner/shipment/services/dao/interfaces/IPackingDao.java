package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Packing;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IPackingDao {
    Packing save(Packing packing);
    Page<Packing> findAll(Specification<Packing> spec, Pageable pageable);
    Optional<Packing> findById(Long id);
    void delete(Packing packing);
    List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId) throws Exception;
}
