package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IViewsDao;
import com.dpw.runner.shipment.services.entity.Views;
import com.dpw.runner.shipment.services.repository.interfaces.IViewsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public class ViewsDao implements IViewsDao {
    @Autowired
    private IViewsRepository viewsRepository;

    @Override
    public Views save(Views views) {
        return viewsRepository.save(views);
    }

    @Override
    public Page<Views> findAll(Specification<Views> spec, Pageable pageable) {
        return viewsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Views> findById(Long id) {
        return viewsRepository.findById(id);
    }

    @Override
    public void delete(Views views) {
        viewsRepository.delete(views);
    }

    @Override
    public List<Views> findAll() {
        return viewsRepository.findAll();
    }

    @Override
    public List<String> findAllByUsername(String username) {
        return viewsRepository.findAllByUsername(username);
    }

    @Override
    public Optional<Views> findByCreatedByAndEntityAndIsDefault(String createdBy, String entity) {
        return Optional.ofNullable(viewsRepository.findByCreatedByAndIsDefault(createdBy, entity));
    }
}
