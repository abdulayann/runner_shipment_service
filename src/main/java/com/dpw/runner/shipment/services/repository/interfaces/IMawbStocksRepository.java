package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
@Generated
public interface IMawbStocksRepository extends MultiTenancyRepository<MawbStocks> {
    Page<MawbStocks> findAll(Specification<MawbStocks> spec, Pageable pageable);

    default Optional<MawbStocks> findByGuid(UUID id) {
        Specification<MawbStocks> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }
}
