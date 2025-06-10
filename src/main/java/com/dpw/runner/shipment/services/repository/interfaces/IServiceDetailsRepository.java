package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Generated
public interface IServiceDetailsRepository extends MultiTenancyRepository<ServiceDetails> {
    Page<ServiceDetails> findAll(Specification<ServiceDetails> spec, Pageable pageable);
    List<ServiceDetails> findByShipmentId(Long shipmentId);
    default Optional<ServiceDetails> findById(Long id) {
        Specification<ServiceDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<ServiceDetails> findAll();

    default Optional<ServiceDetails> findByGuid(UUID id) {
        Specification<ServiceDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    List<ServiceDetails> findByIdIn(List<Long> packingIds);

    @Query(value = "SELECT * FROM services WHERE id = ?1", nativeQuery = true)
    Optional<ServiceDetails> findByIdWithQuery(Long id);

    @Query(value = "SELECT * FROM services WHERE guid = ?1", nativeQuery = true)
    Optional<ServiceDetails> findByGuidWithQuery(UUID guid);

    @ExcludeTenantFilter
    default Page<ServiceDetails> findAllWithoutTenantFilter(Specification<ServiceDetails> spec, Pageable pageable) {
        return findAll(spec, pageable);
    }
}
