package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.DefaultViews;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface IDefaultViewsDao extends JpaRepository<DefaultViews, Long> {
    Optional<DefaultViews> findByDefaultViewId(Long defaultViewId);
    Optional<DefaultViews> findByUsername(String username);
}
