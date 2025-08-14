package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

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

    @Modifying
    @Transactional
    @Query(value = "UPDATE notes SET is_deleted = true WHERE id NOT IN (?1) and entity_id = ?2 and entity_type = ?3", nativeQuery = true)
    void deleteAdditionalNotesByEntityIdAndEntityType(List<Long> notesIds, Long entityId, String entityType);

    @Modifying
    @Transactional
    @Query(value = "UPDATE notes SET is_deleted = false WHERE id IN (?1) and entity_id = ?2 and entity_type = ?3", nativeQuery = true)
    void revertSoftDeleteByNotesIdsAndEntityIdAndEntityType(List<Long> notesIds, Long entityId, String entityType);
}
