package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.FileRepo;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface IFileRepoRepository extends JpaRepository<FileRepo, Long> {
    List<FileRepo> findByEntityIdAndEntityType(Long entityId, String entityType);
    Page<FileRepo> findAll(Specification<FileRepo> spec, Pageable pageable);
    List<FileRepo> findAll();
    Optional<FileRepo> findById(Long id);
}
