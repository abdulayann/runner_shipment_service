package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IDefaultViewsDao;
import com.dpw.runner.shipment.services.entity.DefaultViews;
import com.dpw.runner.shipment.services.repository.interfaces.IDefaultViewsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public class DefaultViewsDao implements IDefaultViewsDao {
    @Autowired
    private IDefaultViewsRepository defaultViewsRepository;

    @Override
    public DefaultViews save(DefaultViews defaultViews) {
        return defaultViewsRepository.save(defaultViews);
    }

    @Override
    public List<DefaultViews> findAll() {
        return defaultViewsRepository.findAll();
    }

    @Override
    public Optional<DefaultViews> findById(Long id) {
        return defaultViewsRepository.findById(id);
    }

    @Override
    public void delete(DefaultViews defaultViews) {
        defaultViewsRepository.delete(defaultViews);
    }

    @Override
    public Optional<DefaultViews> findByDefaultViewId(Long defaultViewId) {
        return defaultViewsRepository.findByDefaultViewId(defaultViewId);
    }

    @Override
    public Optional<DefaultViews> findByUsername(String username) {
        return defaultViewsRepository.findByUsername(username);
    }
}
