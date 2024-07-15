package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Views;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

@Generated
public interface IViewsRepository extends MultiTenancyRepository<Views> {
    List<Views> findAll();
    Page<Views> findAll(Specification<Views> spec, Pageable pageable);
    default Optional<Views> findById(Long id) {
        Specification<Views> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    @Query(value = "SELECT name from views where created_by = ?1 and is_deleted = FALSE", nativeQuery = true)
    List<String> findAllByUsername(String username);

    @Query(value = "SELECT * from views where created_by = ?1 and is_default = TRUE and entity = ?2 and is_deleted = FALSE", nativeQuery = true)
    Views findByCreatedByAndIsDefault(String createdBy, String entity);
}
