package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.response.consolidation.IContainerLiteResponse;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Generated
public interface IContainerRepository extends MultiTenancyRepository<Containers> {
    List<Containers> findAll();
    Page<Containers> findAll(Specification<Containers> spec, Pageable pageable);
    List<Containers> findByGuid(UUID guid);
    default Optional<Containers> findById(Long id) {
        Specification<Containers> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<Containers> findByConsolidationId(Long consolidationId);
    void deleteById(Long id);

    @Modifying
    @Transactional @ExcludeTenantFilter
    @Query(value = "Update containers set pra_status = ?1 where guid = ?2 and consolidation_id = ?3", nativeQuery = true)
    void savePraStatus(String praStatus, UUID guid, Long consolidationId);

    List<Containers> findByConsolidationIdIn(List<Long> consolidationIds);

    List<Containers> findByIdIn(List<Long> containerIds);

    @Query(value = "SELECT c.consolidation_id AS consolidationId, c.container_code AS containerCode, c.container_number AS containerNumber, c.container_count AS containerCount " +
            "FROM containers c WHERE c.consolidation_id IN :consolidationIds", nativeQuery = true)
    List<IContainerLiteResponse> findAllLiteContainer(@Param("consolidationIds") List<Long> consolidationId);
}
