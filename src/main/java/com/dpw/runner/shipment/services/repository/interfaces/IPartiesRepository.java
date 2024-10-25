package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository @Generated
public interface IPartiesRepository extends MultiTenancyRepository<Parties> {
    Page<Parties> findAll(Specification<Parties> spec, Pageable pageable);
    List<Parties> findByEntityIdAndEntityType(Long entityId, String entityType);
    default Optional<Parties> findById(Long id) {
        Specification<Parties> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<Parties> findAll();

    List<Parties> findByIdIn(List<Long> ids);

    @Modifying
    @Query(value = "DELETE FROM parties WHERE id >= ?1 and id <= ?2", nativeQuery = true)
    int deleteById1(Long start, Long end);

    @Modifying
    @Query(value = "DELETE FROM parties WHERE id = ?1", nativeQuery = true)
    int deleteByIdOneId(Long start);
}
