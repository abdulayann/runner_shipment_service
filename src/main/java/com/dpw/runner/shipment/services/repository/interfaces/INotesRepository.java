package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
@Generated
public interface INotesRepository extends MultiTenancyRepository<Notes> {

    Page<Notes> findAll(Specification<Notes> left, Pageable right);
    default Optional<Notes> findById(Long id) {
        Specification<Notes> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    public List<Notes> findByEntityIdAndEntityType(Long entityId, String entityType);
    List<Notes> findAll();

}
