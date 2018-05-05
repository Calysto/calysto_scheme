export VERSION=`python3 setup.py --version 2>/dev/null`

release: 
	rm -rf dist
	python3 setup.py register
	python3 setup.py bdist_wheel --universal
	python3 setup.py sdist
	git commit -a -m "Release $(VERSION)"; true
	git tag v$(VERSION)
	git push origin --all
	git push origin --tags
	twine upload dist/*
