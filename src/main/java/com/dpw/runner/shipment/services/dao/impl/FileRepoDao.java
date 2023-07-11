package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IFileRepoDao;
import com.dpw.runner.shipment.services.entity.FileRepo;
import com.dpw.runner.shipment.services.repository.interfaces.IFileRepoRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public class FileRepoDao implements IFileRepoDao {
    @Autowired
    private IFileRepoRepository fileRepoRepository;

    @Override
    public FileRepo save(FileRepo fileRepo) {
        return fileRepoRepository.save(fileRepo);
    }

    @Override
    public Page<FileRepo> findAll(Specification<FileRepo> spec, Pageable pageable) {
        return fileRepoRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<FileRepo> findById(Long id) {
        return fileRepoRepository.findById(id);
    }

    @Override
    public void delete(FileRepo fileRepo) {
        fileRepoRepository.delete(fileRepo);
    }

    @Override
    public List<FileRepo> findByEntityIdAndEntityType(Long entityId, String entityType) {
        return fileRepoRepository.findByEntityIdAndEntityType(entityId, entityType);
    }
}
