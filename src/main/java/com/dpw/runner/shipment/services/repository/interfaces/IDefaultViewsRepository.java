package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.DefaultViews;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

//TODO- make this tenant specific as well
public interface IDefaultViewsRepository extends JpaRepository<DefaultViews, Long> {
    Optional<DefaultViews> findByDefaultViewId(Long defaultViewId);
    Optional<DefaultViews> findByUsername(String username);
}
