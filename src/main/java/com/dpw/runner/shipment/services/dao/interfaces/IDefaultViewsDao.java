package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.DefaultViews;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IDefaultViewsDao {
    DefaultViews save(DefaultViews defaultViews);
    List<DefaultViews> findAll();
    Optional<DefaultViews> findById(Long id);
    void delete(DefaultViews defaultViews);
    Optional<DefaultViews> findByDefaultViewId(Long defaultViewId);
    Optional<DefaultViews> findByUsername(String username);
    Optional<DefaultViews> findByGuid(UUID guid);
}
