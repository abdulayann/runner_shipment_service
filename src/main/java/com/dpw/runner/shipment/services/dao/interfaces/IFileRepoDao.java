package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.FileRepo;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IFileRepoDao {
    FileRepo save(FileRepo fileRepo);
    Page<FileRepo> findAll(Specification<FileRepo> spec, Pageable pageable);
    Optional<FileRepo> findById(Long id);
    void delete(FileRepo fileRepo);
    List<FileRepo> findByEntityIdAndEntityType(Long entityId, String entityType);
    List<FileRepo> updateEntityFromOtherEntity(List<FileRepo> fileRepoList, Long entityId, String entityType) throws Exception;
    List<FileRepo> saveEntityFromOtherEntity(List<FileRepo> fileRepos, Long entityId, String entityType);
}
