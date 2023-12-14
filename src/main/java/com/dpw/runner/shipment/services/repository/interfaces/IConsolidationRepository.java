package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;


@Repository
public interface IConsolidationRepository extends MultiTenancyRepository<ConsolidationDetails> {
    List<ConsolidationDetails> findAll();
    Page<ConsolidationDetails> findAll(Specification<ConsolidationDetails> spec, Pageable pageable);
    default Optional<ConsolidationDetails> findById(Long id) {
        Specification<ConsolidationDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    default Optional<ConsolidationDetails> findByGuid (UUID guid) {
        Specification<ConsolidationDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), guid);
        return findOne(spec);
    }
    List<ConsolidationDetails> findByBol (String bol);
    List<ConsolidationDetails> findByReferenceNumber(String ref);

    @Query(value = "SELECT MAX(c.id) FROM consolidation_details c", nativeQuery = true)
    Long findMaxId();
}
