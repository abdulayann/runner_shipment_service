package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.FileRepo;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Repository @Generated
public interface IFileRepoRepository extends MultiTenancyRepository<FileRepo> {
    List<FileRepo> findByEntityIdAndEntityType(Long entityId, String entityType);
    Page<FileRepo> findAll(Specification<FileRepo> spec, Pageable pageable);
    List<FileRepo> findAll();
    default Optional<FileRepo> findById(Long id) {
        Specification<FileRepo> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    @Modifying
    @Transactional
    @Query(value = "UPDATE file_repo SET is_deleted = true WHERE id NOT IN (?1) and entity_id = ?2 and entity_type = ?3", nativeQuery = true)
    void deleteAdditionalDataByFileRepoIdsEntityIdAndEntityType(List<Long> fileRepoIds, Long entityId, String entityType);

    @Modifying
    @Transactional
    @Query(value = "UPDATE file_repo SET is_deleted = false WHERE id IN (?1) and entity_id = ?2 and entity_type = ?3", nativeQuery = true)
    void revertSoftDeleteByFileRepoIdsEntityIdAndEntityType(List<Long> fileRepoIds, Long entityId, String entityType);
}
