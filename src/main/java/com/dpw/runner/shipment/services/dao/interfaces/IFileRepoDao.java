package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.FileRepo;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IFileRepoDao {
    FileRepo save(FileRepo fileRepo);
    Page<FileRepo> findAll(Specification<FileRepo> spec, Pageable pageable);
    Optional<FileRepo> findById(Long id);
    void delete(FileRepo fileRepo);
    List<FileRepo> findByEntityIdAndEntityType(Long entityId, String entityType);
    List<FileRepo> updateEntityFromShipment(List<FileRepo> fileRepoList, Long shipmentId) throws Exception;
    Optional<FileRepo> findByGuid(UUID guid);
}
