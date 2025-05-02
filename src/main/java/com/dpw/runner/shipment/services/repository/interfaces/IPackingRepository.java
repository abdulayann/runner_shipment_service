package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.InterBranchEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
@Generated @InterBranchEntity
public interface IPackingRepository extends MultiTenancyRepository<Packing> {

    List<Packing> findByShipmentId(Long shipmentId);

    default Optional<Packing> findById(Long id) {
        Specification<Packing> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<Packing> findAll();

    Page<Packing> findAll(Specification<Packing> spec, Pageable pageable);

    default Optional<Packing> findByGuid(UUID id) {
        Specification<Packing> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    List<Packing> findByConsolidationId(Long consolidationId);

    List<Packing> findByContainerIdIn(List<Long> deleteContainerIds);

    List<Packing> findByIdIn(List<Long> packingIds);
}
